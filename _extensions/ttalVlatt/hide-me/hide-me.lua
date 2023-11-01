-- Title: hide-me Quarto extension
-- Auth: Matt Capaldi (@ttalVlatt)
-- Init: July 31st 2023
-- Updated: July 31st 2023
-- Purpose: Hides content in output, but processes later that .content-hidden
-- in order to work with other extensions such as section-bibliographies
-- after the Quarto AST node update in v1.3

-- Hat Tips
-- h/t cderv https://github.com/quarto-dev/quarto-cli/issues/5904

Div = function(div) 
    if div.classes:includes("hide-me") then
        return {}
    end
end
