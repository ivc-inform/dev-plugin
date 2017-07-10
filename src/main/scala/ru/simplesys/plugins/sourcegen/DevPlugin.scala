package ru.simplesys.plugins
package sourcegen

//import sbt.{`package` => _, _}

import com.simplesys.file.{Path, PathSet}
import sbt.{`package` ⇒ _, _}
import sbt.Keys._
import sbt.classpath.ClasspathUtilities
import liquibase.Liquibase
import liquibase.database.Database
import liquibase.integration.commandline.CommandLineUtils
import liquibase.resource.FileSystemResourceAccessor
import ru.simplesys.plugins.sourcegen.app.AppDef
import net.sf.saxon.lib.FeatureKeys
import ru.simplesys.meta.types.DataTypes
import ru.simplesys.plugins.sourcegen.meta.SchemaDef
import ru.simplesys.plugins.sourcegen.app.Gen.GenDSs

object DevPlugin extends AutoPlugin {
    override lazy val projectSettings: Seq[Setting[_]] = devPluginGeneratorSettings

    val indentSpaces = 4

    val DevConfig = config("dev-config")

    val sourceSchemaDir = settingKey[File]("Source directory for SourceGen files. Defaults to sourceDirectory/main/schema")
    val sourceMainDir = settingKey[File]("Source main directory. Defaults to sourceDirectory/main")
    val tmpResourcesDir = settingKey[File]("Source directory for SourceGen files. Defaults to sourceDirectory/main/resources/defs/tmp")
    val sourceAppDir = settingKey[File]("Source directory for SourceGen WebApp` files. Defaults to sourceDirectory/main/schema/app")
    val sourceBoDir = settingKey[File]("Source directory for Bo` files. Defaults to sourceDirectory/main/resources/defs/bo")
    val sourceMockupUIDir = settingKey[File]("Source directory for UI files. Defaults to sourceDirectory/main/resources/defs/ui")
    val outputScalaCodeDir = settingKey[File]("Directory where the scala files should be placed. Defaults to sourceManaged/defs/")
    val outputCreateChangelogDir = settingKey[File]("Directory where create changelog xml should be placed. Defaults to sourceManaged/migration/create/")
    val outputUpgradeChangelogDir = settingKey[File]("Path to directory with upgrade changelogs. Defaults to resourceDirectory/migration/upgrade/")
    val outputJavaScriptDir = settingKey[File]("Path to directory with generated javascript files. Defaults to resourceManaged/javascript/")
    val outputUIDir = settingKey[File]("Path to directory with generated UI xml files. Defaults to sourceDirectory/main/resources/defs/ui-generated")
    val startPackageName = settingKey[String]("Package name for generation's starting point")
    val contextPath = settingKey[String]("Context WebApp")
    val maxArity = settingKey[Int]("maxArity: How max arity TuplesSS")
    //val isGenerateBOCode = settingKey[Boolean]("Should we generate BO files")
    //val isGenerateJSCode = settingKey[Boolean]("Should we generate Scala->JS files")

    val liquibaseUrl = settingKey[String]("The url for liquibase")
    val liquibaseUsername = settingKey[String]("Username.")
    val liquibasePassword = settingKey[String]("Password")
    val liquibaseDriver = settingKey[String]("DB Driver")
    val liquibaseDefaultSchemaName = settingKey[Option[String]]("Default schema name")

    val quoted = settingKey[Boolean]("Use quotes for generating and using tables, columns, constraints")
    val useDbPrefix = settingKey[Boolean]("Use DB Prefix from DataType[T] for generete columnName in DB Tables")

    //---------------------------------------------------------------------------------
    val liquibaseCreateChangelog = settingKey[File]("This is your liquibase create changelog file. Defaults to sourceManaged/migration/create/db.changelog-create.xml. Shouldn't be changed!")
    val liquibaseUpgradeChangelog = settingKey[File]("This is your liquibase upgrade changelog file. Defaults to sourceManaged/migration/create/db.changelog-create.xml. Shouldn't be changed!")

    val sourceSchemaBOFiles = settingKey[Seq[File]]("These are xml files with BO descriptions. Defaults to sourceSchemaDir/bo/**.xml. Shouldn't be changed!")
    val outputScalaCodeBODir = settingKey[File]("Directory where the BO scala files should be placed. Defaults to outputScalaCodeDir/bo/. Shouldn't be changed!")
    val startPackageBOName = settingKey[String]("Package name for BO generation's starting point. Defaults to startPackageName.bo. Shouldn't be changed!")

    val sourceAppFiles = settingKey[Seq[File]]("These are xml files with WebApp` descriptions. Defaults to sourceAppDir/**.xml. Shouldn't be changed!")
    val outputScalaCodeAppDir = settingKey[File]("Directory where the WebApp scala files should be placed. Defaults to outputScalaCodeDir/app/. Shouldn't be changed!")
    val startPackageAppName = settingKey[String]("Package name for WebApp generation's starting point. Defaults to startPackageName.app. Shouldn't be changed!")

    val sourceMockupUIFiles = settingKey[Seq[File]]("These are xml files with BO descriptions. Defaults to sourceSchemaDir/bo/**.xml. Shouldn't be changed!")

    val liquibaseChangelog = settingKey[File]("This is your liquibase changelog file to run. Defaults to outputUpgradeChangelogDir/db.changelog-upgrade.xml. Shouldn't be changed!")
    val liquibaseContext = settingKey[String]("ChangeSet contexts to execute")
    lazy val liquibaseDatabase = taskKey[Database]("Liquibase database object")
    lazy val liquibase = taskKey[Liquibase]("Liquibase object")

    val generateOnPackage = taskKey[Seq[File]]("internal task to run before packaging")
    //val generateOnCompile = taskKey[Seq[File]]("internal task to run from sourceGenerators (i.e. before compile)")
    //---------------------------------------------------------------------------------

    val generateScalaCode = taskKey[Seq[File]]("Generate scala sources from schema files")
    val generateBoScalaCode = taskKey[Seq[File]]("Generate scala sources tables & classes from schema files")
    val N877 = taskKey[Unit]("Issue #877")
    val generateMockupUI = taskKey[Unit]("generate UI from Balsamiq mockups")
    val logMetamodel = taskKey[Unit]("Incpection BO MetaModel")
    //val logedBos = settingKey[Seq[String]]("Bo of Metamodel")

    val generateCreateChangelog = taskKey[Seq[File]]("Generate liquibase changelog for creation DB objects from schema files")
    val generateUpgradeChangelog = taskKey[Unit]("Generate liquibase changelog for upgrade DB objects from schema files and previous upgrade scripts")
    val generateJavaScript = taskKey[Seq[File]]("Generate javascript sources from schema files")
    //---------------------------------------------------------------------------------
    val generateAll = taskKey[Seq[File]]("Generate scala, javascript sources and all liquibase changelogs from schema files")
    val generateAllButUpgrade = taskKey[Seq[File]]("Generate scala, javascript sources and creation liquibase changelogs from schema files. Upgrade changelogs won't be created")
    val liquibaseUpdate = taskKey[Unit]("Run a liquibase migration - update")
    val liquibaseCreate = taskKey[Unit]("Run a liquibase migration - create")

    //---------------------------------------------------------------------------------

    lazy val devPluginGeneratorSettings: Seq[Setting[_]] = inConfig(DevConfig)(Seq[Setting[_]](
        // Default settings
        //---------------------------------------------------------------------------------
        sourceSchemaDir := {
            (sourceDirectory in Compile).value / "schema"
        },
        sourceMainDir := (sourceDirectory in Compile).value,
        tmpResourcesDir := {
            (sourceManaged in Compile).value / "defs" / "app" / "tmp"
        },
        sourceAppDir := {
            sourceSchemaDir.value / "app"
        },
        outputScalaCodeDir := {
            (sourceManaged in Compile).value / "defs"
        },
        outputCreateChangelogDir := {
            (sourceManaged in Compile).value / "migration" / "create"
        },
        outputUpgradeChangelogDir := {
            (resourceDirectory in Compile).value / "migration" / "upgrade"
        },
        outputJavaScriptDir := {
            (resourceManaged in Compile).value / "javascript"
        },
        startPackageName := {
            organization.value + "." + name.value
        },

        //quoted := true,
        //useDbPrefix := true,
        //maxArity := 254,

        //---------------------------------------------------------------------------------

        sourceSchemaBOFiles <<= (sourceSchemaDir) {
            x => {
                (x / "bo") ** "*.xml"
            }.get
        },
        sourceMockupUIDir <<= (sourceSchemaDir) {
            _ / "ui"
        },
        sourceMockupUIFiles <<= (sourceMockupUIDir) {
            x => {
                x ** "*.bmml"
            }.get
        },
        outputUIDir <<= (sourceSchemaDir) {
            _ / "ui-generated"
        },

        sourceBoDir <<= (sourceSchemaDir) {
            _ / "bo"
        },
        outputScalaCodeBODir <<= (outputScalaCodeDir) {
            _ / "bo"
        },
        startPackageBOName <<= (startPackageName) {
            _ + ".bo"
        },
        sourceAppFiles <<= (sourceSchemaDir) {
            x => {
                (x / "app") * "*.xml"
            }.get
        },
        outputScalaCodeAppDir <<= (outputScalaCodeDir) {
            _ / "app" / "generated"
        },
        startPackageAppName <<= (startPackageName) {
            _ + ".app"
        },

        liquibaseCreateChangelog <<= (outputCreateChangelogDir) {
            _ / "db.changelog-create.xml"
        },
        liquibaseUpgradeChangelog <<= (outputUpgradeChangelogDir) {
            _ / "db.changelog-upgrade.xml"
        },
        liquibaseChangelog := liquibaseUpgradeChangelog.value,
        liquibaseContext := "",
        liquibaseDefaultSchemaName := None,
        //---------------------------------------------------------------------------------

        // Internal structures initialization
        //---------------------------------------------------------------------------------
        liquibaseDatabase := {
                CommandLineUtils.createDatabaseObject(ClasspathUtilities.toLoader(fullClasspath.value.map(_.data)), liquibaseUrl.value, liquibaseUsername.value, liquibasePassword.value, liquibaseDriver.value, null, liquibaseDefaultSchemaName.value.getOrElse(null), null, null)
        },
        liquibase <<= (liquibaseChangelog, liquibaseDatabase) map {
            (cLog: File, dBase: Database) => new Liquibase(cLog.getPath, new FileSystemResourceAccessor, dBase)
        },
        //---------------------------------------------------------------------------------

        // Tasks implementations
        //---------------------------------------------------------------------------------
        liquibaseUpdate <<= (liquibase, liquibaseContext) map {
            (liquibase: Liquibase, context: String) => liquibase.update(context)
        },
        liquibaseCreate <<= (liquibaseCreateChangelog, liquibaseDatabase, liquibaseContext) map {
            (cLog: File, dBase: Database, context: String) =>
                val liquibase = new Liquibase(cLog.getPath, new FileSystemResourceAccessor, dBase)
                liquibase.update(context)
        },
        generateScalaCode := {

            import meta.SchemaDef
            import com.simplesys.file.ImplicitConversions._

            implicit val logger = streams.value.log
            implicit val schema = SchemaDef(startPackageBOName.value, useDbPrefix.value, sourceSchemaBOFiles.value)

            tmpResourcesDir.value.mkdirs()

            val cl2Save = Thread.currentThread.getContextClassLoader
            val cl2Set = this.getClass.getClassLoader

            try {
                Thread.currentThread setContextClassLoader cl2Set
                //                schema.generateScalaCode(outScalaBODir, pkgBOName) ++
                AppDef.generateScalaCode(
                    baseDirectory = baseDirectory.value,
                    tmp = tmpResourcesDir.value,
                    sourceBoDir = sourceBoDir.value,
                    sourceAppDir = sourceAppDir.value,
                    outScalaAppDir = outputScalaCodeAppDir.value,
                    sourceMain = sourceMainDir.value,
                    pkgAppName = startPackageAppName.value,
                    pkgBOName = startPackageBOName.value,
                    contextPath = contextPath.value,
                    maxArity = maxArity.value,
                    useDbPrefix = useDbPrefix.value
                )
            }
            finally {
                Thread.currentThread setContextClassLoader cl2Save
            }

        },

        generateBoScalaCode := {

            import meta.SchemaDef
            import ru.simplesys.plugins.sourcegen.app.Gen.{GenTables, GenBOs, GenEnums}
            import com.simplesys.file.ImplicitConversions._

            implicit val logger = streams.value.log
            implicit val schema = SchemaDef(startPackageBOName.value, useDbPrefix.value, sourceSchemaBOFiles.value)

            val _outDir: Path = outputScalaCodeBODir.value

            val poso = schema.generateScalaCode(outputScalaCodeBODir.value, startPackageBOName.value)

            val res764 = new GenTables(
                appFilePath = sourceBoDir.value,
                outFilePath = _outDir / "table",
                packageName = startPackageBOName.value,
                pkgBOName = startPackageBOName.value,
                quoted = quoted.value,
                useDbPrefix = useDbPrefix.value,
                logger = logger
            ).createSeq

            val res819 = new GenBOs(
                appFilePath = sourceBoDir.value,
                outFilePath = _outDir / "classBo",
                packageName = startPackageBOName.value,
                pkgBOName = startPackageBOName.value,
                quoted = quoted.value,
                stage = "#819",
                useDbPrefix = useDbPrefix.value,
                logger = logger
            ).createSeq

            val res844 = new GenEnums(
                appFilePath = sourceBoDir.value,
                outFilePath = _outDir / "classEnum",
                packageName = startPackageBOName.value,
                pkgBOName = startPackageBOName.value,
                quoted = quoted.value,
                stage = "#844",
                useDbPrefix = useDbPrefix.value,
                logger = logger
            ).createSeq

            val res938 = new GenDSs(
                appFilePath = sourceBoDir.value,
                outFilePath = _outDir / "dataSets",
                packageName = startPackageBOName.value,
                pkgBOName = startPackageBOName.value,
                quoted = quoted.value,
                useDbPrefix = useDbPrefix.value,
                logger = logger
            ).createSeq

            poso ++ res764 ++ res819 ++ res844 ++ res938
        },

        N877 := {

            import com.simplesys.file.ImplicitConversions._
            import com.simplesys.saxon._
            import com.simplesys.saxon.XsltTransformer._
            import com.simplesys.io._

            val logger = streams.value.log
            val _tmp: Path = tmpResourcesDir.value
            val _sourceAppDir: Path = sourceAppDir.value

            val xslPath: Path = _sourceAppDir / "xsl"
            val macroPath: Path = _sourceAppDir / "macroBo"
            var sourceBOFiles: PathSet[Path] = sourceBoDir.value * "*.xml"

            val schema = SchemaDef(startPackageBOName.value, useDbPrefix.value, sourceBOFiles.files)

            XmlUtil.save(schema.toXML("http://toucan.simplesys.lan/xml/xsd"), (_tmp / "allBo.xml").toFile)
            XmlUtil.save(DataTypes.toXML("http://toucan.simplesys.lan/xml/xsd"), (_tmp / "domains.xml").toFile)

            val cl2Save = Thread.currentThread.getContextClassLoader
            val cl2Set = this.getClass.getClassLoader

            try {
                Thread.currentThread setContextClassLoader cl2Set
                if (withTransformation((FeatureKeys.MULTIPLE_SCHEMA_IMPORTS -> true)) {
                    params =>
                        params("inputBoFile") = _tmp / "allBo.xml"
                        params("macroDir") = macroPath
                        Transform(xsltPath = xslPath / "InitialMacroOfClasses.xsl", initialTemplate = "ProcessingAll")
                } > 0)
                    throw new RuntimeException("Execution terminated, due to an error(s) # 877 !!!")
                else
                    logger info (s"Done #877.")
            }
            finally {
                Thread.currentThread setContextClassLoader cl2Save
            }
        },

        generateMockupUI := {

            import balsamiq._

            implicit val logger = streams.value.log
            val schema = SchemaDef(startPackageBOName.value, useDbPrefix.value, sourceSchemaBOFiles.value)
            val uiGenerator = UIGenerator(schema, sourceMockupUIFiles.value)

            uiGenerator generateFiles outputUIDir.value
        },

        generateCreateChangelog := {

            import meta.SchemaDef

            implicit val logger = streams.value.log
            val schema = SchemaDef(startPackageBOName.value, useDbPrefix.value, sourceSchemaBOFiles.value)
            schema generateCreateChangelog liquibaseCreateChangelog.value
        },

        generateUpgradeChangelog := {
                implicit val logger = streams.value.log
                LiquibaseUpgradeGen.generateUpgradeChangelog(outputUpgradeChangelogDir.value, liquibaseCreateChangelog.value, liquibaseUpgradeChangelog.value, baseDirectory.value)
        },

        generateJavaScript := {
            Seq()
        },

        generateAllButUpgrade := {

                import meta.SchemaDef

                implicit val logger = streams.value.log
                val schema = SchemaDef( startPackageBOName.value, useDbPrefix.value, sourceSchemaBOFiles.value)
                schema.generateScalaCode(outputScalaCodeBODir.value,  startPackageBOName.value)

        },

        generateOnPackage <<= (streams, sourceSchemaBOFiles, startPackageBOName, outputCreateChangelogDir, liquibaseCreateChangelog, outputUpgradeChangelogDir, liquibaseUpgradeChangelog, baseDirectory /*, outputJavaScriptDir*/ , useDbPrefix) map {
            (out, sourceBOFiles, pkgBOName, outCreateChLogDir, createChLogFile, outUpgradeChLogDir, upgradeChLogFile, baseDir /*, outJSDir*/ , useDbPrefix) => {

                import meta.SchemaDef

                implicit val logger = out.log
                val schema = SchemaDef(pkgBOName, useDbPrefix, sourceBOFiles)
                LiquibaseUpgradeGen.generateUpgradeChangelog(outUpgradeChLogDir, createChLogFile, upgradeChLogFile, baseDir)
                Seq.empty[File]
            }
        } runBefore (`package` in Compile),

        //todo На кой мы апдейтим managedClasspath?
        managedClasspath := {
            Classpaths.managedJars(DevConfig, classpathTypes.value, update.value)
        }
    )) ++ Seq[Setting[_]](
        watchSources <++= (sourceSchemaDir in DevConfig) map {
            (tdir) => (tdir ** "*.xml").get
        },
        //todo А это зачем мы апдейтим?
        ivyConfigurations += DevConfig
    )
}
