package wom.values

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import wom.WomExpressionException
import wom.types._

import scala.util.{Success, Try}

class WomFileSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "WomFile"

  lazy val singleDir = WomSingleDirectory("single/dir")
  lazy val singleFile = WomSingleFile("single/file")
  lazy val globFile = WomGlobFile("glob/*")
  lazy val listedDir1 = WomSingleDirectory("listed/dir1")
  lazy val listedDir2 = WomSingleDirectory("listed/dir2")
  lazy val secondaryFile1 = WomSingleFile("secondary/file1")
  lazy val secondaryFile2 = WomSingleFile("secondary/file2")
  lazy val dirWithListedDirs = singleDir.copy(listingOption = Option(Seq(listedDir1, listedDir2)))
  lazy val dirWithListedFiles = singleDir.copy(listingOption = Option(Seq(secondaryFile1, secondaryFile2)))
  lazy val fileWithSecondaryFiles = singleFile.copy(secondaryFiles = List(secondaryFile1, secondaryFile2))
  lazy val fileWithSecondaryDirs = singleFile.copy(secondaryFiles = Seq(listedDir1, listedDir2))
  lazy val nestedFilesAndDirs = singleFile.copy(secondaryFiles = Seq(
    listedDir1.copy(listingOption = Option(Seq(
      secondaryFile1.copy(secondaryFiles = Seq(
        listedDir2.copy(listingOption = Option(Seq(secondaryFile2)))
      ))
    )))
  ))

  val mapFileTests = Table(
    ("description", "womFile", "expected"),
    ("a single directory", singleDir, WomSingleDirectory("prepend/single/dir")),
    ("a single file", singleFile, WomSingleFile("prepend/single/file")),
    ("a glob file", globFile, WomGlobFile("prepend/glob/*")),
    ("a dir with listed dirs", dirWithListedDirs,
      WomSingleDirectory("prepend/single/dir", Option(
        Seq(WomSingleDirectory("prepend/listed/dir1"), WomSingleDirectory("prepend/listed/dir2"))
      ))
    ),
    ("a dir with listed files", dirWithListedFiles,
      WomSingleDirectory("prepend/single/dir", Option(
        Seq(WomSingleFile("prepend/secondary/file1"), WomSingleFile("prepend/secondary/file2"))
      ))
    ),
    ("a file with secondary files", fileWithSecondaryFiles,
      WomSingleFile("prepend/single/file", None, None, None, None,
        Seq(WomSingleFile("prepend/secondary/file1"), WomSingleFile("prepend/secondary/file2"))
      )
    ),
    ("a file with secondary dirs", fileWithSecondaryDirs,
      WomSingleFile("prepend/single/file", None, None, None, None,
        Seq(WomSingleDirectory("prepend/listed/dir1"), WomSingleDirectory("prepend/listed/dir2"))
      )
    ),
    ("a nested file/dir", nestedFilesAndDirs,
      WomSingleFile("prepend/single/file", None, None, None, None,
        Seq(WomSingleDirectory("prepend/listed/dir1",
          Option(Seq(WomSingleFile("prepend/secondary/file1", None, None, None, None,
            Seq(WomSingleDirectory("prepend/listed/dir2",
              Option(Seq(WomSingleFile("prepend/secondary/file2")))
            ))
          )))
        ))
      )
    )
  )

  forAll(mapFileTests) { (description, womFile, expected) =>
    it should s"map $description" in {
      womFile.mapFile("prepend/" + _) should be(expected)
    }
  }

  val flattenFileTests = Table(
    ("description", "womFile", "expected"),
    ("a single directory", singleDir, Seq(WomSingleDirectory("single/dir"))),
    ("a single file", singleFile, Seq(WomSingleFile("single/file"))),
    ("a glob file", globFile, Seq(WomGlobFile("glob/*"))),
    ("a dir with listed dirs", dirWithListedDirs,
      Seq(
        WomSingleDirectory("listed/dir1"),
        WomSingleDirectory("listed/dir2")
      )
    ),
    ("a dir with listed files", dirWithListedFiles,
      Seq(
        WomSingleFile("secondary/file1"),
        WomSingleFile("secondary/file2")
      )
    ),
    ("a file with secondary files", fileWithSecondaryFiles,
      Seq(
        WomSingleFile("single/file", None, None, None, None,
          Seq(WomSingleFile("secondary/file1"), WomSingleFile("secondary/file2"))
        ),
        WomSingleFile("secondary/file1"),
        WomSingleFile("secondary/file2")
      )
    ),
    ("a file with secondary dirs", fileWithSecondaryDirs,
      Seq(
        WomSingleFile("single/file", None, None, None, None,
          Seq(WomSingleDirectory("listed/dir1"), WomSingleDirectory("listed/dir2"))
        ),
        WomSingleDirectory("listed/dir1"),
        WomSingleDirectory("listed/dir2")
      )
    ),
    ("a nested file/dir", nestedFilesAndDirs,
      Seq(
        WomSingleFile("single/file", None, None, None, None,
          Seq(WomSingleDirectory("listed/dir1",
            Option(Seq(WomSingleFile("secondary/file1", None, None, None, None,
              Seq(WomSingleDirectory("listed/dir2",
                Option(Seq(WomSingleFile("secondary/file2")))
              ))
            )))
          ))
        ),
        WomSingleFile("secondary/file1", None, None, None, None,
          Seq(WomSingleDirectory("listed/dir2",
            Option(Seq(WomSingleFile("secondary/file2")))
          ))
        ),
        WomSingleFile("secondary/file2")
      )
    )
  )

  forAll(flattenFileTests) { (description, womFile, expected) =>
    it should s"flatten $description" in {
      womFile.flattenFiles should be(expected)
    }
  }

  it should "test valueString" in {
    singleDir.valueString should be("single/dir")
    singleFile.valueString should be("single/file")
    globFile.valueString should be("glob/*")
  }

  it should "test womString" in {
    singleDir.toWomString should be(""""single/dir"""")
    singleFile.toWomString should be(""""single/file"""")
    globFile.toWomString should be("""glob("glob/*")""")
  }

  it should "create wom files based on types" in {
    WomFile(WomSingleDirectoryType, "example") should be(WomSingleDirectory("example"))
    WomFile(WomSingleFileType, "example") should be(WomSingleFile("example"))
    WomFile(WomGlobFileType, "example") should be(WomGlobFile("example"))
  }

  val addPrefixTests = Table(
    ("description", "womFile", "expected"),
    ("a single directory", singleDir, WomString("prefix/single/dir")),
    ("a single file", singleFile, WomString("prefix/single/file")),
    ("a glob file", globFile, WomString("prefix/glob/*")),
    ("a dir with listed dirs", dirWithListedDirs, WomString("prefix/single/dir")),
    ("a dir with listed files", dirWithListedFiles, WomString("prefix/single/dir")),
    ("a file with secondary files", fileWithSecondaryFiles, WomString("prefix/single/file")),
    ("a file with secondary dirs", fileWithSecondaryDirs, WomString("prefix/single/file")),
    ("a nested file/dir", nestedFilesAndDirs, WomString("prefix/single/file"))
  )

  forAll(addPrefixTests) { (description, womFile, expected) =>
    it should s"add a string prefix to $description" in {
      WomString("prefix/").add(womFile) should be(Success(expected))
      WomOptionalValue(WomStringType, Option(WomString("prefix/"))).add(
        WomOptionalValue(womFile.womType, Option(womFile))
      ) should be(Success(expected))
    }
  }

  val addSuffixTests = Table(
    ("description", "womFile", "expected"),
    ("a single directory", singleDir, WomSingleDirectory("single/dir/suffix")),
    ("a single file", singleFile, WomSingleFile("single/file/suffix")),
    ("a glob file", globFile, WomGlobFile("glob/*/suffix")),
    ("a dir with listed dirs", dirWithListedDirs, WomSingleDirectory("single/dir/suffix")),
    ("a dir with listed files", dirWithListedFiles, WomSingleDirectory("single/dir/suffix")),
    ("a file with secondary files", fileWithSecondaryFiles, WomSingleFile("single/file/suffix")),
    ("a file with secondary dirs", fileWithSecondaryDirs, WomSingleFile("single/file/suffix")),
    ("a nested file/dir", nestedFilesAndDirs, WomSingleFile("single/file/suffix"))
  )

  forAll(addSuffixTests) { (description, womFile, expected) =>
    it should s"add a string suffix to $description" in {
      womFile.add(WomString("/suffix")) should be(Success(expected))
      WomOptionalValue(womFile.womType, Option(womFile)).add(
        WomOptionalValue(WomStringType, Option(WomString("/suffix")))
      ) should be(Success(expected))
    }
  }

  val addInvalidIntegerTests = Table(
    ("description", "womFile", "expected"),
    ("a single directory", singleDir, "Cannot perform operation: single/dir + WomInteger(42)"),
    ("a single file", singleFile, "Cannot perform operation: single/file + WomInteger(42)"),
    ("a glob file", globFile, "Cannot perform operation: glob/* + WomInteger(42)")
  )

  forAll(addInvalidIntegerTests) { (description, womFile, expected) =>
    it should s"fail to add an int to $description" in {
      womFile.add(WomInteger(42)).failed.get.getMessage should be(expected)
      WomOptionalValue(womFile.womType, Option(womFile)).add(
        WomOptionalValue(WomIntegerType, Option(WomInteger(42)))
      ).failed.get.getMessage should be(expected)
    }
  }

  val womFileEqualsTests = Table(
    ("description", "womFileA", "womFileB", "expected"),
    ("a single directory matched to a similar directory", singleDir, WomSingleDirectory(singleDir.value), true),
    ("a single file matched to a similar directory", singleFile, WomSingleDirectory(singleDir.value), false),
    ("a glob file matched to a similar directory", globFile, WomSingleDirectory(singleDir.value), false),
    ("a single directory matched to a similar file", singleDir, WomSingleFile(singleFile.value), false),
    ("a single file matched to a similar file", singleFile, WomSingleFile(singleFile.value), true),
    ("a glob file matched to a similar file", globFile, WomSingleFile(singleFile.value), false),
    ("a single directory matched to a similar glob", singleDir, WomGlobFile(globFile.value), false),
    ("a single file matched to a similar glob", singleFile, WomGlobFile(globFile.value), false),
    ("a glob file matched to a similar glob", globFile, WomGlobFile(globFile.value), true),
    ("a dir with listed dirs matched to a similar directory", dirWithListedDirs, WomSingleDirectory(singleDir.value),
      true),
    ("a dir with listed files matched to a similar directory", dirWithListedFiles, WomSingleDirectory(singleDir.value),
      true),
    ("a file with secondary files matched to a similar file", fileWithSecondaryFiles, WomSingleFile(singleFile.value),
      true),
    ("a file with secondary dirs matched to a similar file", fileWithSecondaryDirs, WomSingleFile(singleFile.value),
      true),
    ("a nested file/dir matched to a similar file", nestedFilesAndDirs, WomSingleFile(singleFile.value), true)
  )

  forAll(womFileEqualsTests) { (description, womFileA, womFileB, expected) =>
    it should s"expect $expected comparing $description" in {
      womFileA.equals(womFileB) should be(Success(WomBoolean(expected)))
      womFileB.equals(womFileA) should be(Success(WomBoolean(expected)))
      WomOptionalValue(womFileA.womType, Option(womFileA)).equals(
        WomOptionalValue(womFileB.womType, Option(womFileB))
      ) should be(Success(WomBoolean(expected)))
      WomOptionalValue(womFileB.womType, Option(womFileB)).equals(
        WomOptionalValue(womFileA.womType, Option(womFileA))
      ) should be(Success(WomBoolean(expected)))
    }
  }

  val womStringEqualsTests = Table(
    ("description", "womFile", "string", "expected"),
    ("a single directory matched to a similar directory", singleDir, singleDir.value, true),
    ("a single file matched to a similar directory", singleFile, singleDir.value, false),
    ("a glob file matched to a similar directory", globFile, singleDir.value, false),
    ("a single directory matched to a similar file", singleDir, singleFile.value, false),
    ("a single file matched to a similar file", singleFile, singleFile.value, true),
    ("a glob file matched to a similar file", globFile, singleFile.value, false),
    ("a single directory matched to a similar glob", singleDir, globFile.value, false),
    ("a single file matched to a similar glob", singleFile, globFile.value, false),
    ("a glob file matched to a similar glob", globFile, globFile.value, true),
    ("a dir with listed dirs matched to a similar directory", dirWithListedDirs, singleDir.value, true),
    ("a dir with listed files matched to a similar directory", dirWithListedFiles, singleDir.value, true),
    ("a file with secondary files matched to a similar file", fileWithSecondaryFiles, singleFile.value, true),
    ("a file with secondary dirs matched to a similar file", fileWithSecondaryDirs, singleFile.value, true),
    ("a nested file/dir matched to a similar file", nestedFilesAndDirs, singleFile.value, true)
  )

  forAll(womStringEqualsTests) { (description, womFile, string, expected) =>
    it should s"expect $expected comparing $description as a string" in {
      womFile.equals(WomString(string)) should be(Success(WomBoolean(expected)))
      WomOptionalValue(womFile.womType, Option(womFile)).equals(
        WomOptionalValue(WomStringType, Option(WomString(string)))
      ) should be(Success(WomBoolean(expected)))

      WomString(string).equals(womFile).failed.get should be(a[WomExpressionException])
      WomOptionalValue(WomStringType, Option(WomString(string))).equals(
        WomOptionalValue(womFile.womType, Option(womFile))
      ).failed.get should be(a[WomExpressionException])
    }
  }

  it should "produce an invalid equals" in {
    val result: Try[WomBoolean] = singleFile.equals(WomInteger(42))
    result.failed.get.getMessage should be("Cannot perform operation: single/file == WomInteger(42)")
  }
}
