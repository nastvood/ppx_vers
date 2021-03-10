type%vers tp =
  [ `A1
  | `A2 of int
  ]

type%vers[@novers] tpn =
  [ `B1
  | `B2 of tp
  ]

type%vers[@novers] tpn1 =
  [ `C1 of int
  | `C2 of tp
  ] and tpn1 =
  [ `C1 of float [@migrate ? `C1 p when `C1 (float_of_int p) ]
  | `C2 of tp
  ]
