module Localization

type Locale =
    | RU
    | EN

type Localization =
    { defaultValue: string
      locales: Map<Locale, string> }

let translate_from defaultLocale defaultValue locales =
    { defaultValue = defaultValue
      locales = Map(locales @ [ defaultLocale, defaultValue ]) }

let ru text = translate_from RU text []

let translate_to localization locale =
    match localization.locales.TryFind locale with
    | Some value -> value
    | _ -> localization.defaultValue

let to_ru localization = translate_to localization RU
