import sys.process._


object Eprover {
  val PATH_TO_EPROVER = "/home/jcmaas/Documents/eprover/PROVER/eprover"

  def execute(filename: String) = PATH_TO_EPROVER + " --auto " + filename !!
}
