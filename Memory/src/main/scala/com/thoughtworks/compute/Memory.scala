package com.thoughtworks.compute

import java.nio._

import org.lwjgl.PointerBuffer
import org.lwjgl.system.{CustomBuffer, MemoryUtil, Pointer}

import scala.reflect.ClassTag

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
trait Memory[Element] {
  type HostBuffer

  def fromByteBuffer(byteBuffer: ByteBuffer): HostBuffer

  def numberOfBytesPerElement: Int

  def address(buffer: HostBuffer): Long

  def remaining(buffer: HostBuffer): Int

  def remainingBytes(buffer: HostBuffer): Int = numberOfBytesPerElement * remaining(buffer)

  def get(buffer: HostBuffer, index: Int): Element

  def put(buffer: HostBuffer, index: Int, value: Element): Unit

  def allocate(numberOfElement: Int): HostBuffer

  def free(buffer: HostBuffer): Unit

  def toArray(buffer: HostBuffer): Array[Element]
}

object Memory extends LowPriorityMemory {

  final val CHAR_ALLOC_DEFAULT_BYTE_SIZE = 2

  def apply[Element](implicit memory: Memory[Element]): memory.type = memory

  type Aux[Element, HostBuffer0] = Memory[Element] {
    type HostBuffer = HostBuffer0
  }

  trait NioMemory[Element] extends Memory[Element] {
    type HostBuffer <: java.nio.Buffer

    override def remaining(buffer: HostBuffer): Int = buffer.remaining
  }

  trait CustomMemory[Element] extends Memory[Element] {
    type HostBuffer <: CustomBuffer[HostBuffer]

    override def remaining(buffer: HostBuffer): Int = buffer.remaining

    override def address(buffer: HostBuffer): Long = (buffer.address)
  }

  implicit object PointerMemory extends CustomMemory[Pointer] {
    override type HostBuffer = PointerBuffer

    override def numberOfBytesPerElement: Int = Pointer.POINTER_SIZE

    override def fromByteBuffer(byteBuffer: ByteBuffer): PointerBuffer = {
      PointerBuffer.create(byteBuffer)
    }

    override def get(buffer: PointerBuffer, index: Int): Pointer = new Pointer.Default(buffer.get(index)) {}

    override def put(buffer: PointerBuffer, index: Int, value: Pointer): Unit = buffer.put(index, value)

    override def allocate(numberOfElement: Int): PointerBuffer = MemoryUtil.memAllocPointer(numberOfElement)

    override def free(buffer: PointerBuffer): Unit = MemoryUtil.memFree(buffer)

    override def toArray(buffer: PointerBuffer): Array[Pointer] = {
      val oldPosition = buffer.position()
      val bufferToArray = Array.ofDim[Long](buffer.remaining())
      buffer.get(bufferToArray, 0, bufferToArray.length)
      buffer.position(oldPosition)
      bufferToArray.map { long =>
        new Pointer.Default(long) {}
      }
    }
  }

//  implicit object HNilMemory extends NioMemory[HNil] {
//    override type HostBuffer = ByteBuffer
//
//    override def fromByteBuffer(byteBuffer: ByteBuffer): ByteBuffer = byteBuffer
//
//    override def numberOfBytesPerElement: Int = 0
//
//    override def address(buffer: ByteBuffer): Long = MemoryUtil.memAddress(buffer)
//
//    override def free(buffer: ByteBuffer): Unit = MemoryUtil.memFree(buffer)
//
//    override def get(buffer: ByteBuffer, index: Int): HNil = HNil
//
//    override def put(buffer: ByteBuffer, index: Int, value: HNil): Unit = {}
//
//    override def allocate(numberOfElement: Int): ByteBuffer = MemoryUtil.memAlloc(1)
//
//  }

  implicit object IntMemory extends NioMemory[Int] {
    override type HostBuffer = IntBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): IntBuffer = byteBuffer.asIntBuffer

    override def numberOfBytesPerElement: Int = java.lang.Integer.BYTES

    override def address(buffer: IntBuffer): Long = MemoryUtil.memAddress(buffer)

    override def allocate(numberOfElement: Int): IntBuffer = MemoryUtil.memAllocInt(numberOfElement)

    override def free(buffer: IntBuffer): Unit = MemoryUtil.memFree(buffer)

    override def get(buffer: IntBuffer, index: Int): Int = buffer.get(index)

    override def put(buffer: IntBuffer, index: Int, value: Int): Unit = buffer.put(index, value)

    override def toArray(buffer: IntBuffer): Array[Int] = {
      val oldPosition = buffer.position()
      val bufferToArray = Array.ofDim[Int](buffer.remaining())
      buffer.get(bufferToArray, 0, bufferToArray.length)
      buffer.position(oldPosition)
      bufferToArray
    }
  }

  implicit object LongMemory extends NioMemory[Long] {
    override type HostBuffer = LongBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): LongBuffer = byteBuffer.asLongBuffer

    override def numberOfBytesPerElement: Int = java.lang.Long.BYTES

    override def address(buffer: LongBuffer): Long = MemoryUtil.memAddress(buffer)

    override def allocate(numberOfElement: Int): LongBuffer = MemoryUtil.memAllocLong(numberOfElement)

    override def free(buffer: LongBuffer): Unit = MemoryUtil.memFree(buffer)

    override def get(buffer: LongBuffer, index: Int): Long = buffer.get(index)

    override def put(buffer: LongBuffer, index: Int, value: Long): Unit = buffer.put(index, value)

    override def toArray(buffer: LongBuffer): Array[Long] = {
      val oldPosition = buffer.position()
      val bufferToArray = Array.ofDim[Long](buffer.remaining())
      buffer.get(bufferToArray, 0, bufferToArray.length)
      buffer.position(oldPosition)
      bufferToArray
    }
  }

  implicit object DoubleMemory extends NioMemory[Double] {
    override type HostBuffer = DoubleBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): DoubleBuffer = byteBuffer.asDoubleBuffer

    override def numberOfBytesPerElement: Int = java.lang.Double.BYTES

    override def address(buffer: DoubleBuffer): Long = MemoryUtil.memAddress(buffer)

    override def allocate(numberOfElement: Int): DoubleBuffer = MemoryUtil.memAllocDouble(numberOfElement)

    override def free(buffer: DoubleBuffer): Unit = MemoryUtil.memFree(buffer)

    override def get(buffer: DoubleBuffer, index: Int): Double = buffer.get(index)

    override def put(buffer: DoubleBuffer, index: Int, value: Double): Unit = buffer.put(index, value)

    override def toArray(buffer: DoubleBuffer): Array[Double] = {
      val oldPosition = buffer.position()
      val bufferToArray = Array.ofDim[Double](buffer.remaining())
      buffer.get(bufferToArray, 0, bufferToArray.length)
      buffer.position(oldPosition)
      bufferToArray
    }
  }

  implicit object FloatMemory extends NioMemory[Float] {
    override type HostBuffer = FloatBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): FloatBuffer = byteBuffer.asFloatBuffer

    override def numberOfBytesPerElement: Int = java.lang.Float.BYTES

    override def address(buffer: FloatBuffer): Long = MemoryUtil.memAddress(buffer)

    override def allocate(numberOfElement: Int): FloatBuffer = MemoryUtil.memAllocFloat(numberOfElement)

    override def free(buffer: FloatBuffer): Unit = MemoryUtil.memFree(buffer)

    override def get(buffer: FloatBuffer, index: Int): Float = buffer.get(index)

    override def put(buffer: FloatBuffer, index: Int, value: Float): Unit = buffer.put(index, value)

    override def toArray(buffer: FloatBuffer): Array[Float] = {
      val oldPosition = buffer.position()
      val bufferToArray = new Array[Float](buffer.remaining())
      buffer.get(bufferToArray, 0, bufferToArray.length)
      buffer.position(oldPosition)
      bufferToArray
    }
  }

  implicit object ByteMemory extends NioMemory[Byte] {
    override type HostBuffer = ByteBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): ByteBuffer = byteBuffer

    override def numberOfBytesPerElement: Int = java.lang.Byte.BYTES

    override def address(buffer: ByteBuffer): Long = MemoryUtil.memAddress(buffer)

    override def allocate(numberOfElement: Int): ByteBuffer = MemoryUtil.memAlloc(numberOfElement)

    override def free(buffer: ByteBuffer): Unit = MemoryUtil.memFree(buffer)

    override def get(buffer: ByteBuffer, index: Int): Byte = buffer.get(index)

    override def put(buffer: ByteBuffer, index: Int, value: Byte): Unit = buffer.put(index, value)

    override def toArray(buffer: ByteBuffer): Array[Byte] = {
      val oldPosition = buffer.position()
      val bufferToArray = Array.ofDim[Byte](buffer.remaining())
      buffer.get(bufferToArray, 0, bufferToArray.length)
      buffer.position(oldPosition)
      bufferToArray
    }
  }

  implicit object ShortMemory extends NioMemory[Short] {
    override type HostBuffer = ShortBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): ShortBuffer = byteBuffer.asShortBuffer()

    override def numberOfBytesPerElement: Int = java.lang.Short.BYTES

    override def address(buffer: ShortBuffer): Long = MemoryUtil.memAddress(buffer)

    override def allocate(numberOfElement: Int): ShortBuffer = MemoryUtil.memAllocShort(numberOfElement)

    override def free(buffer: ShortBuffer): Unit = MemoryUtil.memFree(buffer)

    override def get(buffer: ShortBuffer, index: Int): Short = buffer.get(index)

    override def put(buffer: ShortBuffer, index: Int, value: Short): Unit = buffer.put(index, value)

    override def toArray(buffer: ShortBuffer): Array[Short] = {
      val oldPosition = buffer.position()
      val bufferToArray = Array.ofDim[Short](buffer.remaining())
      buffer.get(bufferToArray, 0, bufferToArray.length)
      buffer.position(oldPosition)
      bufferToArray
    }
  }

  // TODO: short, bool, char

  implicit object CharMemory extends NioMemory[Char] {

    override type HostBuffer = CharBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): CharBuffer = byteBuffer.asCharBuffer()

    override def numberOfBytesPerElement: Int = java.lang.Character.BYTES

    override def address(buffer: CharBuffer): Long = MemoryUtil.memAddress(buffer)

    override def get(buffer: CharBuffer, index: Int): Char = buffer.get(index)

    override def put(buffer: CharBuffer, index: Int, value: Char): Unit = buffer.put(index, value)

    override def allocate(numberOfElement: Int): CharBuffer = MemoryUtil.memAlloc(CHAR_ALLOC_DEFAULT_BYTE_SIZE * numberOfElement).asCharBuffer()

    override def free(buffer: CharBuffer): Unit = MemoryUtil.memFree(buffer)

    override def toArray(buffer: CharBuffer): Array[Char] = {
      val oldPosition = buffer.position()
      val bufferToArray = Array.ofDim[Char](buffer.remaining())
      buffer.get(bufferToArray, 0, bufferToArray.length)
      buffer.position(oldPosition)
      bufferToArray
    }
  }

  trait Box[Boxed] {
    type Raw

    def box(raw: Raw): Boxed

    def unbox(boxed: Boxed): Raw
  }

  object Box {
    type Aux[Boxed, Raw0] = Box[Boxed] {
      type Raw = Raw0
    }
  }

  final class BoxedMemory[Raw, Boxed, HostBuffer0](implicit box: Box.Aux[Boxed, Raw],
                                                   rawMemory: Memory.Aux[Raw, HostBuffer0],
                                                   classTag: ClassTag[Boxed])
      extends Memory[Boxed] {
    override type HostBuffer = HostBuffer0

    override def fromByteBuffer(byteBuffer: ByteBuffer): HostBuffer = {
      rawMemory.fromByteBuffer(byteBuffer)
    }

    override def numberOfBytesPerElement: Int = {
      rawMemory.numberOfBytesPerElement
    }

    override def remaining(buffer: HostBuffer): Int = {
      rawMemory.remaining(buffer)
    }

    override def get(buffer: HostBuffer, index: Int): Boxed = {
      box.box(rawMemory.get(buffer, index))
    }

    override def put(buffer: HostBuffer, index: Int, value: Boxed): Unit = {
      rawMemory.put(buffer, index, box.unbox(value))
    }

    override def address(buffer: HostBuffer): Long = {
      rawMemory.address(buffer)
    }

    override def allocate(numberOfElement: Int): HostBuffer0 = rawMemory.allocate(numberOfElement)

    override def free(buffer: HostBuffer0): Unit = rawMemory.free(buffer)

    override def toArray(buffer: HostBuffer0): Array[Boxed] = {
//      ???
      val rawArray: Array[Raw] = rawMemory.toArray(buffer)
      val boxedArray = new Array[Boxed](rawArray.length)
      var i = 0
      while (i < rawArray.length) {
        boxedArray(i) = box.box(rawArray(i))
        i += 1
      }
      boxedArray
    }
  }

}

private[compute] trait LowPriorityMemory {
  this: Memory.type =>

  implicit def boxedMemory[Raw, Boxed, HostBuffer0](implicit box: Box.Aux[Boxed, Raw],
                                                    rawMemory: Memory.Aux[Raw, HostBuffer0],
                                                    classTag: ClassTag[Boxed]): BoxedMemory[Raw, Boxed, HostBuffer0] = {
    new BoxedMemory[Raw, Boxed, HostBuffer0]
  }

}
