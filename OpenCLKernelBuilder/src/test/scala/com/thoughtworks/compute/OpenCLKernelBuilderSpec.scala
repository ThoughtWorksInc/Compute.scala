package com.thoughtworks.compute

import com.dongxiguo.fastring.Fastring
import com.thoughtworks.compute.Expressions.{Arrays, Floats, Tuples}
import com.thoughtworks.compute.OpenCLKernelBuilder.GlobalContext
import com.thoughtworks.compute.Trees.AllTrees
import com.thoughtworks.feature.Factory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author 杨博 (Yang Bo)
  */
class OpenCLKernelBuilderSpec extends AnyFreeSpec with Matchers {

  "id" in {
    val category1 = {
      Factory[AllTrees].newInstance()
    }

    val category2 = {
      Factory[AllTrees].newInstance()
    }

    def foo(e1: category1.FloatTerm): category2.FloatTerm = {
      e1.in(category2)
    }

    def bar(e1: category1.ArrayTerm { type Element = category1.FloatTerm })
      : category2.ArrayTerm { type Element = category2.FloatTerm } = {
      e1.in(category2)
    }

  }

  "opencl" in {

    val trees2: AllTrees { type Category = Tuples with Floats with Arrays } = {
      Factory[AllTrees].newInstance()
    }

    val trees: AllTrees { type Category = Tuples with Floats with Arrays } = {
      Factory[AllTrees].newInstance()
    }

    val x: trees.ArrayTerm { type Element = trees.FloatTerm } =
      trees.array.parameter("x", trees.float.literal(0.0f), Array(32, 32))
    val y: trees.ArrayTerm { type Element = trees.FloatTerm } =
      trees.array.parameter("y", trees.float.literal(0.0f), Array(32, 32))

    val filledZeros = trees.float.literal(0.0f).fill.extract

    val f: trees.FloatTerm = x.extract

    val globalContext = new GlobalContext
    val openCLFunctionContext = Factory[OpenCLKernelBuilder].newInstance(globalContext)

    val map = new trees.ExportContext

    val sourceCode: Fastring =
      openCLFunctionContext.generateKernelSourceCode(
        "kernel_name",
        2,
        3,
        Seq(
          x.tree.export(openCLFunctionContext, map),
          y.tree.export(openCLFunctionContext, map)
        ),
        Seq(f.tree.export(openCLFunctionContext, map))
      )

    globalContext.foreach(print)
    sourceCode.foreach(print)

    // TODO: Convert this example to a test case

  }
}
