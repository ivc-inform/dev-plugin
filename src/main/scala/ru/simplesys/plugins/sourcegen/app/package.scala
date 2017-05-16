package ru.simplesys.plugins.sourcegen

import java.net.URI

package object app {
    val uriXSD = "http://toucan.simplesys.lan/xml/xsd/v1.0.0-1/"

    implicit class StringOpt(value: String) {
        def xsdURI: URI = new URI(uriXSD + value)
    }
}
