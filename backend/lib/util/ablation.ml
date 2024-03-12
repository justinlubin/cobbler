let enabled_ref : bool ref = ref false
let enable () = enabled_ref := true
let enabled () = !enabled_ref
