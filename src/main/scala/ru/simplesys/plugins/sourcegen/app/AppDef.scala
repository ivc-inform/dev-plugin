package ru.simplesys.plugins.sourcegen.app

import sbt.{Logger, File}
import scalax.file.{PathSet, Path}
import scala.collection.mutable.ArrayBuffer
import net.sf.saxon.lib.FeatureKeys
import com.simplesys.saxon.XsltTransformer._
import com.simplesys.saxon._
import ru.simplesys.plugins.sourcegen.app.Gen._
import ru.simplesys.plugins.sourcegen.XmlUtil
import ru.simplesys.meta.types.DataTypes
import ru.simplesys.plugins.sourcegen.meta.SchemaDef
import com.simplesys.io._

object AppDef {
    def generateScalaCode(tmp: Path, sourceBoDir: Path, sourceAppDir: Path, outScalaAppDir: Path, sourceMain: Path, pkgAppName: String, pkgBOName: String, contextPath: String, maxArity: Int)(implicit logger: Logger): Seq[File] = {
        if (contextPath.isEmpty)
            throw new RuntimeException(s"ContextPath must be not Empty.")
        //Path("journal").deleteRecursively(force = true)
        val res = ArrayBuffer.empty[File]
        val xmlPath: Path = outScalaAppDir / "xml"
        val xslPath: Path = sourceAppDir / "xsl"
        val scalaOut: Path = outScalaAppDir / "scala" / "bo"

        val boFiles: PathSet[Path] = sourceBoDir * "*.xml"
        val appFiles: PathSet[Path] = sourceAppDir / "macroBo" * "*.xml"
        var sourceBOFiles: PathSet[Path] = sourceBoDir * "*.xml"

        val jsDir: Path = sourceMain / "webapp" / "managed" / "javascript" / "common-webapp" / "developed"

        val schema = SchemaDef(pkgBOName, sourceBOFiles.files)

        logger info (s"Begin source generation.")

        //<editor-fold desc="#756">
        logger info (s"Begin #756.")
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

        logger info (s"Begin #932#3.")
        res += new GenSimpleTypes(
            appFilePath = tmp / "SimpleTypes.xml",
            schemaPath = "schemaISC.xsd".xsdURI,
            outFilePath = outScalaAppDir / "scala" / "components" / "SimpleTypes.scala",
            packageName = pkgAppName + ".scala",
            logger = logger).create
        logger info (s"Done #932#3.")

        logger info (s"Done #756.")
        //</editor-fold>

        //<editor-fold desc="#757">
        logger info (s"Begin #757.")
        if (withTransformation((FeatureKeys.MULTIPLE_SCHEMA_IMPORTS -> true)) {
            params =>
                params("ContextPath") = contextPath
                params("resFile") = tmp / "dataSources.xml"
                params("inputBoFile") = tmp / "allBo.xml"
                params("domainsFile") = tmp / "domains.xml"
                params("maxArity") = maxArity
                params("tmpDir") = tmp
                Transform(xsltPath = xslPath / "MakeDSFromAllBo.xsl", initialTemplate = "ProcessingAll")
        } > 0)
            throw new RuntimeException("Execution terminated, due to an error(s) in #756 !!!")
        else
            logger info (s"Done #757.")
        //</editor-fold>

        //<editor-fold desc="#758">
        logger info (s"Begin #758.")
        if (withTransformation((FeatureKeys.MULTIPLE_SCHEMA_IMPORTS -> true)) {
            params =>
                params("ContextPath") = contextPath
                params("tmpDir") = tmp
                params("jsDir") = jsDir
                params("macroDir") = sourceAppDir / "macroBo"
                params("generatedDir") = xmlPath
                params("FilesName") = appFiles
                Transform(xsltPath = xslPath / "BoTransformation.xsl", initialTemplate = "ProcessingAll")
        } > 0)
            throw new RuntimeException("Execution terminated, due to an error(s) in #758 !!!")
        else
            logger info (s"Done #758.")
        //</editor-fold>

        if (!xmlPath.exists) xmlPath.createDirectory()

        val generetedFiles: PathSet[Path] = xmlPath * "*.xml"

        if (generetedFiles.files.length > 0) {
            //<editor-fold desc="#762">
            logger info (s"Begin #762.")
            if (withTransformation((FeatureKeys.MULTIPLE_SCHEMA_IMPORTS -> true)) {
                params =>
                    params("files") = generetedFiles
                    params("tmpDir") = tmp
                    params("jsDir") = jsDir
                    Transform(xsltPath = xslPath / "MakeDynamicPartOfRibbon.xsl", initialTemplate = "MakeDynRibbon")
            } > 0)
                throw new RuntimeException("Execution terminated, due to an error(s) in #762 !!!")
            else
                logger info (s"Done #762.")
            //</editor-fold>
        }

        //<editor-fold desc="#778">
        logger info (s"Begin #778.")
        if (withTransformation((FeatureKeys.MULTIPLE_SCHEMA_IMPORTS -> true)) {
            params =>
                params("tmpDir") = tmp
                params("jsDir") = jsDir
                Transform(xsltPath = xslPath / "EmbeddingDynamicPartOfRibbon.xsl", initialTemplate = "Embedding")
        } > 0)
            throw new RuntimeException("Execution terminated, due to an error(s) in #778 !!!")
        else
            logger info (s"Done #778.")
        //</editor-fold>

        //<editor-fold desc="#760">
        //<editor-fold desc="#761">

        logger info (s"Begin #761.")
        res ++= new GenScalaApp(
            appFilePath = xmlPath,
            schemaPath = "schemaISC.xsd".xsdURI,
            outFilePath = scalaOut,
            packageName = pkgAppName + ".gen.scala",
            logger = logger).createSeq

        logger info (s"Done #761.")
        //</editor-fold>
        //</editor-fold>

        //<editor-fold desc="#763">
        logger info (s"Begin #763.")
        res += new GenMainPage(
            appFilePath = tmp / "FullMainView.xml",
            schemaPath = "schemaISC.xsd".xsdURI,
            outFilePath = outScalaAppDir / "scala" / "servlet" / "MainPageServlet.scala",
            packageName = pkgAppName + ".scala.mainPage",
            logger = logger).create
        logger info (s"Done #763.")
        //</editor-fold>

        //<editor-fold desc="#765">
        logger info (s"Begin #765.")

        res ++= new GenBOServlet(
            appFilePath = xmlPath,
            boFilePath = sourceBoDir,
            schemaPath = "schemaApp.xsd".xsdURI,
            outFilePath = outScalaAppDir,
            packageName = pkgAppName + ".scala.servlet",
            pkgBOName,
            logger = logger).createSeq

        logger info (s"Done #765.")
        //</editor-fold>

        logger info (s"Source generation done.")
        res
    }
}