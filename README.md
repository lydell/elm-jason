# Jason

This is a re-implementation of [elm/json](https://package.elm-lang.org/packages/elm/json/latest/) in Elm (rather than JavaScript).

I did this to learn more deeply how JSON decoding and encoding really works. I’ve tried to comment the code to teach about it – check it out!

I looked at these files of elm/json when making this package:

- [src/Json/Decode.elm](https://github.com/elm/json/blob/af344039e8c014b06ed0f73ac3ffd22c60d30876/src/Json/Decode.elm)
- [src/Json/Encode.elm](https://github.com/elm/json/blob/af344039e8c014b06ed0f73ac3ffd22c60d30876/src/Json/Encode.elm)
- [src/Elm/Kernel/Json.js](https://github.com/elm/json/blob/af344039e8c014b06ed0f73ac3ffd22c60d30876/src/Elm/Kernel/Json.js)

I have some loose plans to maybe use the techniques in this package to create an alternate JSON decoding package that explores a slightly different API and different error messages.

[Elm Discourse topic](https://discourse.elm-lang.org/t/elm-json-re-implemented-in-elm-for-learning/4395)
