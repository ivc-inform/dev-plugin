package ru.simplesys.plugins.sourcegen.app.Gen

import java.net.URI

import com.simplesys.common.Strings._
import com.simplesys.genSources._
import com.simplesys.io._
import com.simplesys.scalaGen._
import com.simplesys.xhtml.XHTML._
import ru.simplesys.plugins.sourcegen.app.xml.IscElem
import sbt.{File, Logger}

import scalax.file.ImplicitConversions._
import scalax.file.Path

class GenMainPage(val appFilePath: Path,
                  val schemaPath: URI,
                  val outFilePath: Path,
                  val packageName: String,
                  val logger: Logger) extends GenScala {

    val boFilePath: Path = ""

    def create: File = {
        val res = outFilePath.createFile(failIfExists = false).toFile

        val root: IscElem = loadFile(appFilePath.toFile, schemaPath)
        //root.log
        val suitMainPage = new ScalaClassDeclare {
            extensibleClass = "Config".ext
            scalaClassGen = "SuiteMainPage".cls
            typeScalaClass = TypeScalaTrait
        }

        val _body = ScalaBody()

        _body ++=(
          newLine,
          ScalaVariable(name = "_Isc", serrializeToOneString = true, body = "IscDyn()".body),
          ScalaVariable(name = "_Isc2", serrializeToOneString = true, body = "IscDyn()".body),
          newLine,
          "_Isc componentEnqueue new PageDyn {setAppImgDir(contextPath +  \"managed/images/common-webapp/app/\")}",
          "_Isc commandEnqueue1(this,\"if (simpleSyS.app.mainPage.mainLayout) simpleSyS.app.mainPage.mainLayout.markForDestroy()\")",
          "_Isc commandEnqueue1(this,\"if (simpleSyS.app.mainPage.tabsSet) simpleSyS.app.mainPage.tabsSet.markForDestroy()\")",
          newLine,
          "_Isc2 componentEnqueue new PageDyn {setAppImgDir(contextPath +  \"managed/images/common-webapp/app/\")}",
          "_Isc2 commandEnqueue1(this,\"if (simpleSyS.app.mainPage.mainLayout) simpleSyS.app.mainPage.mainLayout.markForDestroy()\")",
          "_Isc2 commandEnqueue1(this,\"if (simpleSyS.app.mainPage.tabsSet) simpleSyS.app.mainPage.tabsSet.markForDestroy()\")",
          newLine,
          ScalaVariable(name = "ModeNames", serrializeToOneString = true, body = "PortalMode".body, variableType = AssignVariable),
          newLine,
          ScalaVariable(name = "tabSet", body = makeScalaCode(root \ "TabSetSSDyn")),
          newLine,
          ScalaVariable(name = "functionButton", body = makeScalaCode(root \ "IconMenuButtonDyn")),
          ScalaVariable(name = "functionGroup", body = makeScalaCode(root \ "RibbonGroupDyn")),
          newLine,
          "_Isc commandEnqueue1(this,\"if (simpleSyS.app.mainPage.funcMenuButton) simpleSyS.app.mainPage.funcMenuButton.markForDestroy()\")",
          "_Isc commandEnqueue1(this,\"if (simpleSyS.app.mainPage.functionGroup) simpleSyS.app.mainPage.functionGroup.markForDestroy()\")",
          newLine,
          "_Isc2 commandEnqueue1(this,\"if (simpleSyS.app.mainPage.funcMenuButton) simpleSyS.app.mainPage.funcMenuButton.markForDestroy()\")",
          "_Isc2 componentEnqueue (functionButton, (\"simpleSyS.app.mainPage.funcMenuButton\", true))",
          newLine,
          "_Isc2 commandEnqueue1(this,\"if (simpleSyS.app.mainPage.functionGroup) simpleSyS.app.mainPage.functionGroup.markForDestroy()\")",
          "_Isc2 componentEnqueue (functionGroup, (\"simpleSyS.app.mainPage.functionGroup\", true))",
          newLine,
          "_Isc componentEnqueue (tabSet, (\"simpleSyS.app.mainPage.tabsSet\", true))",
          "_Isc2 componentEnqueue (tabSet, (\"simpleSyS.app.mainPage.tabsSet\", true))",
          newLine,
          ScalaVariable(name = "mainViewNotLoged", body = makeScalaCode(root \ "DataViewSSDyn", Seq("IfNotLogged"))),
          newLine,
          "_Isc componentEnqueue (mainViewNotLoged, (\"simpleSyS.app.mainPage.mainLayout\", true))",
          newLine,
          ScalaVariable(name = "mainViewLoged", body = makeScalaCode(root \ "DataViewSSDyn", Seq("IfLogged"))),
          "_Isc2 componentEnqueue (mainViewLoged, (\"simpleSyS.app.mainPage.mainLayout\", true))",
          newLine,
          "//_Isc.logCommandLine()",
          "//_Isc2.logCommandLine()",
          newLine,
          ScalaIf("getLoged".expr, "logJSActor(_Isc2.getCommandLine)".body, "logJSActor(_Isc.getCommandLine)".body)
          )

        val getpageMethod = ScalaMethod(name = "getPage", parametrs = ScalaClassParametrs(
            serrializeToOneString = false,
            indent = 12,
            ScalaClassParametr(name = "getLoged", `type` = ScalaBoolean),
            ScalaClassParametr(name = "getCaptionUser", `type` = ScalaString),
            ScalaClassParametr(name = "contextPath", `type` = ScalaString)),
            `type` = ScalaString,
            body = ScalaVariable(name = "mainView", body = _body).body,
            parametrsImplicit = ScalaClassParametrs(ScalaClassParametr(name = "system", `type` = ScalaActorSystem, parametrType = ParametrImplicit))
        )

        suitMainPage addMembers (
          getpageMethod
          )

        val dataURL = "/MainPage"

        val module = ScalaModule(
            (packageName + "servlet").pkg,
           "com.simplesys.isc.templates.logJSActor".imp,
           "com.simplesys.config.Config".imp,
           "com.simplesys.isc.system.typesDyn._".imp,
           "com.simplesys.isc.system.{PageDyn, ArrayDyn, IscDyn}".imp,
           "com.simplesys.isc.system.global._".imp,
           "com.simplesys.isc.control.{MenuDyn, IconMenuButtonDyn, IconButtonDyn}".imp,
           "com.simplesys.isc.layout._".imp,
           "com.simplesys.isc.foundation.LabelDyn".imp,
           "com.simplesys.isc.system.misc._".imp,
           "com.simplesys.isc.system.ClassDyn._".imp,
           "com.simplesys.isc.control.menu.MenuItemDyn".imp,
           "com.simplesys.app.SessionContext".imp,
           "com.simplesys.servlet.http.{HttpServletResponse, HttpServletRequest}".imp,
           "com.simplesys.common.Strings._".imp,
           "akka.actor.ActorSystem".imp,
           "com.simplesys.json._".imp,
           "com.simplesys.components.Dyn.Layout.{IconMenuButtonSSDyn, TabSetSSDyn}".imp,
           "com.simplesys.components.Dyn.DataBinding.DataViewSSDyn".imp,
            newLine,
            suitMainPage
        )


        res <== {
            out =>
                out(genMessageCreating("GenMainPage"))
                out(newLine)
                out(newLine)
                out(module.serrialize())
        }
    }
}