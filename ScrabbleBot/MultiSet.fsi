﻿// Insert your MultiSet.fsi file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison>

    val empty : MultiSet<'a>
    val add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b
    // NEW
    val remove : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
