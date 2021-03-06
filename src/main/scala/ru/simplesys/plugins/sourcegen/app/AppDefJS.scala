package ru.simplesys.plugins.sourcegen.app

import com.simplesys.file.{Path, PathSet}
import com.simplesys.io._
import com.simplesys.saxon.XsltTransformer._
import com.simplesys.saxon._
import net.sf.saxon.lib.FeatureKeys
import ru.simplesys.meta.types.DataTypes
import ru.simplesys.plugins.sourcegen.XmlUtil
import ru.simplesys.plugins.sourcegen.app.Gen._
import ru.simplesys.plugins.sourcegen.meta.SchemaDef
import sbt.{File, Logger}

import scala.collection.mutable.ArrayBuffer

object AppDefJS {
    def makeScalaCode(baseDirectory: Path, tmp: Path, sourceBoDir: Path, sourceAppDir: Path, outScalaAppDir: Path, sourceMain: Path, pkgAppName: String, pkgBOName: String, contextPath: String, maxArity: Int)(implicit logger: Logger): Seq[File] = {
        if (contextPath.isEmpty)
            throw new RuntimeException(s"ContextPath must be not Empty.")

        //Path("journal").deleteRecursively(force = true)
        val res = ArrayBuffer.empty[File]
        val xmlPath: Path = outScalaAppDir / "xml"
        val xslPath: Path = sourceAppDir / "xsl"
        val scalaOut: Path = outScalaAppDir / "scala" / "bo"

        val boFiles: PathSet[Path] = sourceBoDir * "*.xml"
        val appFiles: PathSet[Path] = sourceAppDir / "macroBo" * "*.xml"
        val sourceBOFiles: PathSet[Path] = sourceBoDir * "*.xml"

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
                params("resFile") = (tmp / "SimpleTypes.xml").toURL
                params("inputBoFile") = (tmp / "domains.xml").toURL
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
            stage = "#932#3",
            logger = logger).create
        logger info (s"Done #932#3.")

        logger info (s"Done #756.")
        //</editor-fold>

        //<editor-fold desc="#757">
        logger info (s"Begin #757.")
        if (withTransformation((FeatureKeys.MULTIPLE_SCHEMA_IMPORTS -> true)) {
            params =>
                params("ContextPath") = contextPath
                params("resFile") = (tmp / "dataSources.xml").toURL
                params("inputBoFile") = (tmp / "allBo.xml").toURL
                params("domainsFile") = (tmp / "domains.xml").toURL
                params("maxArity") = maxArity
                params("tmpDir") = tmp.toURL
                Transform(xsltPath = xslPath / "MakeDSFromAllBo.xsl", initialTemplate = "ProcessingAll")
        } > 0)
            throw new RuntimeException("Execution terminated, due to an error(s) in #757 !!!")
        else
            logger info (s"Done #757.")
        //</editor-fold>

        //<editor-fold desc="#760">
        //<editor-fold desc="#761">

        logger info (s"Begin #761.")
        res ++= new GenDataSources(
            appFilePath = tmp,
            schemaPath = "schemaISC.xsd".xsdURI,
            outFilePath = scalaOut,
            packageName = pkgAppName + ".gen.scala",
            stage = "#761",
            logger = logger).createSeq
        logger info (s"Done #761.")

        logger info (s"Begin #761.1.")
        res ++= new GenListGridFields(
            appFilePath = tmp,
            schemaPath = "schemaISC.xsd".xsdURI,
            outFilePath = scalaOut,
            packageName = pkgAppName + ".gen.scala",
            stage = "#761.1",
            logger = logger).createSeq

        logger info (s"Done #761.1.")
        //</editor-fold>
        //</editor-fold>

        //<editor-fold desc="#765">
        logger info (s"Begin #765.")
        res ++= new GenBODataRecord(
            appFilePath = tmp,
            boFilePath = sourceBoDir,
            schemaPath = "schemaApp.xsd".xsdURI,
            sourceMain = sourceMain,
            outFilePath = outScalaAppDir,
            packageName = pkgAppName + ".scala.container",
            pkgBOName,
            stage = "#765",
            logger = logger).createSeq

        logger info (s"Done #765.")
        //</editor-fold>

        logger info (s"Source Scala.js generation done.")
        res
    }
}
