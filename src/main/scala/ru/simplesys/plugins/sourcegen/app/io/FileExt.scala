package ru.simplesys.plugins.sourcegen.app.io

import java.io.File

object FileExt {
    implicit class FileOpt(asFile: File) {
        private def newChile(component: String, file: File): File = if (component == ".") asFile else new File(asFile, component)

        def \\(components: String*): File = {
            components.foreach(component ⇒ newChile(component, asFile))
            asFile
        }
    }
}
