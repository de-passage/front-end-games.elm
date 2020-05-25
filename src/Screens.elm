module Screens exposing (xsScreen, smScreen, mdScreen, lgScreen, smScreenOnly, mdScreenOnly)

import Css exposing (Px, px, Style)
import Css.Media as M

smScreenMax : Px
smScreenMax =
    px 768


mdScreenMax : Px
mdScreenMax =
    px 992


lgScreenMin : Px
lgScreenMin =
    px 1200


xsScreen : List Style -> Style
xsScreen =
    M.withMedia [ M.only M.screen [ M.maxWidth smScreenMax ] ]


smScreen : List Style -> Style
smScreen =
    M.withMedia [ M.only M.screen [ M.minWidth mdScreenMax ] ]


mdScreen : List Style -> Style
mdScreen =
    M.withMedia [ M.only M.screen [ M.minWidth lgScreenMin ] ]


smScreenOnly : List Style -> Style
smScreenOnly =
    M.withMedia [ M.all [ M.minWidth smScreenMax, M.maxWidth mdScreenMax ] ]


mdScreenOnly : List Style -> Style
mdScreenOnly =
    M.withMedia [ M.all [ M.minWidth mdScreenMax, M.maxWidth lgScreenMin ] ]


lgScreen : List Style -> Style
lgScreen =
    M.withMedia [ M.only M.screen [ M.minWidth lgScreenMin ] ]