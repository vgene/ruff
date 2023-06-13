# Make sure we get the right brackets and the right indentation
a1 = 1 if False else 2
a2 = "akjsdhfksajhfdlksahflashldsahlfsahlfk" if "ajsdjflhaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" == "kjasdfaskjdfhlsadhflsae" else "aksjdflafdahsfld"

# Comments
b1 = (
    # a
    1  # b
    if # c
    False # d
    else  # e
    2  # f
)
b2 = (
    # a
    1  # b
    if False  # c  # d
    else  # e
    # f
    2  # g
)
