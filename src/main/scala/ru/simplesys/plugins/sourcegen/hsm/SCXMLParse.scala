package ru.simplesys.plugins
package sourcegen
package hsm

import ru.simplesys.plugins.sourcegen.meta.LinkRefToAbstractClass

trait HSMState
trait HSMStateSimple extends HSMState
trait HSMStateComplex extends HSMState
trait HSMStateNested extends HSMStateComplex
trait HSMStateParallel extends HSMStateComplex


trait HSMDef {
  def classRef: LinkRefToAbstractClass

}


object HSMDef
