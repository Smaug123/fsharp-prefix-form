namespace Expression.Internals

[<AutoOpen>]
module ActivePatterns =
    let (|Int|_|) (c : char) : int option =
        if '0' <= c && c <= '9' then
            Some (int(c) - int('0'))
        else None
