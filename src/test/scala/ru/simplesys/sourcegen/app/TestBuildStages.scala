package ru.simplesys.sourcegen.app

import org.scalatest.FunSuite
import com.simplesys.log.Logging
import net.sf.saxon.lib.FeatureKeys
import com.simplesys.saxon._
import com.simplesys.saxon.XsltTransformer._
import scalax.file.{PathSet, Path}
import scalax.file.ImplicitConversions._
import ru.simplesys.plugins.sourcegen.app._
import sbt.{Level, Logger}
import com.simplesys.io._
import ru.simplesys.plugins.sourcegen.app.Gen._
import com.simplesys.saxon.Transform
import ru.simplesys.plugins.sourcegen.XmlUtil
import ru.simplesys.meta.types.DataTypes
import ru.simplesys.plugins.sourcegen.meta.SchemaDef

class TestBuildStages extends FunSuite with Logging {

    //val out: Path = "/home/andrew/JOB/fsm-generation/dev-plugin/src/test/scala"
    //val out: Path = "/home/andrew/JOB/enzo/target/scala-2.10/src_managed"
    val out: Path = "/home/andrew/JOB/dm-processing/target/scala-2.10/src_managed"

    //val sourceBoDir: Path = "/home/andrew/JOB/enzo/src/main/resources/defs/bo"
    val sourceBoDir: Path = "/home/andrew/JOB/dm-processing/src/main/resources/defs/bo"
    val sourceAppDir: Path = "/home/andrew/JOB/dm-processing/src/main/resources/defs/app"
    val outScalaBODir: Path = out / "main" / "defs" / "bo"
    val outScalaAppDir: Path = out / "main" / "defs" / "app" / "generated"
    val tmp: Path = out / "main" / "defs" / "app" / "tmp"

    val useQuotes4Tbls = true
    val pkgBOName = "ru.simplesys.defs.bo"
    val pkgAppName = "ru.simplesys.defs.app.gen"

    val xmlPath: Path = outScalaAppDir / "xml"

    val xslPath: Path = sourceAppDir / "xsl"
    val macroPath: Path = sourceAppDir / "macroBo"
    val scalaOut: Path = outScalaAppDir / "scala" / "bo"

    val boFiles: PathSet[Path] = sourceBoDir * "*.xml"
    val appFiles: PathSet[Path] = macroPath * "*.xml"

    //Земена на выполнение процедуры XmlUtil.save(schema.toXML("../../../../../../../src/main/resources/defs/bo"), tmp / "allBo.xml")
    test("#756") {
        logger info (s"Begin #756.")
        val schema = SchemaDef(pkgBOName, boFiles.files)

        XmlUtil.save(schema.toXML("http://toucan.simplesys.lan/xml/xsd/v1.0.0-1"), (tmp / "allBo.xml").toFile)

        logger info (s"Begin #932#1.")
        XmlUtil.save(DataTypes.toXML("http://toucan.simplesys.lan/xml/xsd/v1.0.0-1"), (tmp / "domains.xml").toFile)
        logger info (s"Done #932#1.")

        logger info (s"Begin #932#2.")
        if (withTransformation((FeatureKeys.MULTIPLE_SCHEMA_IMPORTS -> true)) {
            params =>
                params("resFile") = tmp / "SimpleTypes.xml"
                params("inputBoFile") = tmp / "domains.xml"
                Transform(xsltPath = xslPath / "MakeSimpleClasses.xsl", initialTemplate = "ProcessingAll")
        } > 0)
            throw new RuntimeException("Execution terminated, due to an error(s) !!!")
        else
            logger info (s"Done #932#2.")

//        logger info (s"Begin #932#3.")
//        new GenSimpleTypes(
//            appFilePath = tmp / "SimpleTypes.xml",
//            schemaPath = "schemaISC.xsd".xsdURI,
//            outFilePath = outScalaAppDir / "scala" / "components" / "SimpleTypes.scala",
//            packageName = pkgAppName + ".scala",
//            logger = _logger).create
//        logger info (s"Done #932#3.")
        logger info (s"Done #756.")
    }

    test("#877") {
        logger info (s"Begin #877.")

        if (withTransformation((FeatureKeys.MULTIPLE_SCHEMA_IMPORTS -> true)) {
            params =>
                params("inputBoFile") = tmp / "allBo.xml"
                params("macroDir") = macroPath
                Transform(xsltPath = xslPath / "InitialMacroOfClasses.xsl", initialTemplate = "ProcessingAll")
        } > 0)
            throw new RuntimeException("Execution terminated, due to an error(s) !!!")
        else
            logger info (s"Done #877.")

    }

    test("#757") {
        logger info (s"Begin #757.")
        if (withTransformation((FeatureKeys.MULTIPLE_SCHEMA_IMPORTS -> true)) {
            params =>
                params("ContextPath") = "enzo"
                params("resFile") = tmp / "dataSources.xml"
                params("inputBoFile") = tmp / "allBo.xml"
                params("tmpDir") = tmp
                Transform(xsltPath = xslPath / "MakeDSFromAllBo.xsl", initialTemplate = "ProcessingAll")
        } > 0)
            throw new RuntimeException("Execution terminated, due to an error(s) !!!")
        else
            logger info (s"Done #757.")
    }

    test("#758") {
        logger info (s"Begin #758.")
        //xmlPath.deleteRecursively(force = true)
        if (withTransformation((FeatureKeys.MULTIPLE_SCHEMA_IMPORTS -> true)) {
            params =>
                params("ContextPath") = "enzo"
                params("tmpDir") = tmp
                params("jsDir") = sourceAppDir / "js"
                params("macroDir") = sourceAppDir / "macroBo"
                params("generatedDir") = xmlPath
                params("FilesName") = appFiles
                Transform(xsltPath = xslPath / "BoTransformation.xsl", initialTemplate = "ProcessingAll")
        } > 0)
            throw new RuntimeException("Execution terminated, due to an error(s) !!!")
        else
            logger info (s"Done #758.")
    }

    test("#762") {
        if (xmlPath.exists) {
            logger info (s"Begin #762.")
            val generetedFiles: PathSet[Path] = xmlPath * "*.xml"

            if (withTransformation((FeatureKeys.MULTIPLE_SCHEMA_IMPORTS -> true)) {
                params =>
                    params("files") = generetedFiles
                    params("tmpDir") = tmp
                    params("jsDir") = sourceAppDir / "js"
                    Transform(xsltPath = xslPath / "MakeDynamicPartOfRibbon.xsl", initialTemplate = "MakeDynRibbon")
            } > 0)
                throw new RuntimeException("Execution terminated, due to an error(s) !!!")
            else
                logger info (s"Done #762.")
        }
    }

    test("#778") {
        logger info (s"Begin #778.")
        if (withTransformation((FeatureKeys.MULTIPLE_SCHEMA_IMPORTS -> true)) {
            params =>
                params("tmpDir") = tmp
                params("jsDir") = sourceAppDir / "js"
                Transform(xsltPath = xslPath / "EmbeddingDynamicPartOfRibbon.xsl", initialTemplate = "Embedding")
        } > 0)
            throw new RuntimeException("Execution terminated, due to an error(s) !!!")
        else
            logger info (s"Done #778.")
    }

    implicit val _logger = new Logger {
        def log(level: Level.Value, message: => String) {
            logger debug message
        }
        def success(message: => String) {
            logger debug message
        }
        def trace(t: => Throwable) {
            logger debug t
        }
    }

    test("#761") {
        logger info (s"Begin #761.")

        new GenScalaApp(
            appFilePath = xmlPath,
            schemaPath = "schemaISC.xsd".xsdURI,
            outFilePath = scalaOut,
            packageName = pkgAppName + ".scala",
            logger = _logger).createSeq

        logger info (s"Done #761.")
    }

    test("#763") {
        logger info (s"Begin #763.")
        new GenMainPage(
            appFilePath = tmp / "FullMainView.xml",
            schemaPath = "schemaISC.xsd".xsdURI,
            outFilePath = outScalaAppDir / "scala" / "servlet" / "MainPageServlet.scala",
            packageName = pkgAppName + ".scala.mainPage",
            logger = _logger).create
        logger info (s"Done #763.")
    }

    test("#765") {
        logger info (s"Begin #765.")

        new GenBOServlet(
            appFilePath = xmlPath,
            boFilePath = sourceBoDir,
            schemaPath = "schemaApp.xsd".xsdURI,
            outFilePath = outScalaAppDir,
            packageName = pkgAppName + ".scala.servlet",
            pkgBOName = pkgAppName + ".scala.servlet",
            logger = _logger).createSeq


        logger info (s"Done #765.")
    }

    test("#764") {
        logger info (s"Begin #764.")
        new GenTables(
            appFilePath = sourceBoDir,
            outFilePath = outScalaBODir / "table",
            packageName = pkgBOName,
            pkgBOName = pkgBOName,
            quoted = true,
            logger = _logger
        ).createSeq
        logger info (s"Done #764.")
    }

    test("#819") {
        new GenBOs(
            appFilePath = sourceBoDir,
            outFilePath = outScalaBODir / "classBo",
            packageName = pkgBOName,
            pkgBOName = pkgBOName,
            quoted = true,
            logger = _logger
        ).createSeq
    }

    test("#938") {
        new GenDSs(
            appFilePath = sourceBoDir,
            outFilePath = outScalaBODir / "dataSets",
            packageName = pkgBOName,
            pkgBOName = pkgBOName,
            quoted = true,
            logger = _logger
        ).createSeq
    }

    test("#844") {
        new GenEnums(
            appFilePath = sourceBoDir,
            outFilePath = outScalaBODir / "classEnum",
            packageName = pkgBOName,
            pkgBOName = pkgBOName,
            quoted = true,
            logger = _logger
        ).createSeq
    }
}
