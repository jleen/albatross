from dataclasses import dataclass


PARAGRAPHS = [ 4, 5, 6, 5, 3, 8, 10, 3, 5, 3, 9, 3, 2, 2, 6]
TARGET_HEIGHT = 15

def is_recto(page_num):
    return page_num % 2 == 1

def is_verso(page_num):
    return page_num % 2 == 0


current_page_num = 1
current_page_shape = []
page_shapes = []
is_partial = False
page_remaining = TARGET_HEIGHT

for paragraph_remaining in PARAGRAPHS:
    while paragraph_remaining > 0:
        if page_remaining == 0:
            page_shapes += [current_page_shape]
            current_page_shape = []
            current_page_num += 1
            page_remaining = TARGET_HEIGHT
        elif paragraph_remaining <= page_remaining:
            current_page_shape += [('END' if is_partial else 'ENTIRE',
                                    paragraph_remaining)]
            is_partial = False
            page_remaining -= paragraph_remaining
            paragraph_remaining = 0
        else:
            current_page_shape += [('MID' if is_partial else 'BEGIN',
                                    page_remaining)]
            is_partial = True
            paragraph_remaining -= page_remaining
            page_shapes += [current_page_shape]
            current_page_shape = []
            current_page_num += 1
            page_remaining = TARGET_HEIGHT


for shape in page_shapes:
    print(f'Total page length: {sum([lines for (partype, lines) in shape])}')
    for (partype, lines) in shape:
        widow = ' (widow!)' if partype == 'END' and lines == 1 else ''
        print(f'   {partype} paragraph of {lines} lines{widow}')
