# spago run --main Command.Flatten -a "-i locale"
node flatten.js -i locale
# echo '[["en",[["core.json",{}],["translation.json",{}]]],["hu",[["core.json",{"form.select.emptyOption":"Egyik sem","form.validation.mixed.defined":"Meg kell adni a {{path}} szót","form.validation.mixed.notOneOf":"{{path}} nem tartalmazhatja a következõ értékeket {{values}}","form.validation.mixed.oneOf":"{{path}} értéknek a következõ értékek egyikét kell tartalmaznia {{values}}","form.validation.mixed.required":"{{path}} kötelezõ mezõ","form.validation.mixed.default":"{{path}} érvénytelen","form.validation.this":"ez","essentials.options.heading":"Beállítások"}],["translation.json",{"no":"Nem","yes":"Igen","required":"Kötelező","language":"Nyelv","button.close":"Bezárás","button.next":"Következő","button.back":"Vissza"}]]]]'
# echo '[["en",[["core.json",{}]]]]'