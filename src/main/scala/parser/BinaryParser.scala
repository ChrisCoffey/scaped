package org.scaped.parser

import org.scaped.Resp

trait BinaryParser[T] {

    def parse(binFile: BinaryFile): Resp[T]

}
