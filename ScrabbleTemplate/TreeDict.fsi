module TreeDict

    type TreeDict
    val empty : unit -> TreeDict
    val lookup : string -> TreeDict -> bool
    val insert : string -> TreeDict -> TreeDict
    val step : char -> TreeDict -> (bool * TreeDict) option