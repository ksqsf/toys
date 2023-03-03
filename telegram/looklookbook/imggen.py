# imggen.py - Generate images from HTML templates

# Copyright (C) 2022-2023 ksqsf

# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:

# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import imgkit
from jinja2 import Template

def generate_card(banner_image_url, avatar, name, date, today, streak, total, outfile):
    with open('card_template.html', 'r') as f:
        template = Template(f.read())
    s = template.render(
        banner_image_url=banner_image_url,
        avatar=avatar,
        name=name,
        date=date,
        today=today,
        streak=streak,
        total=total
    )
    options = {
        'width': 400
    }
    imgkit.from_string(s, outfile, options=options)

generate_card(
    'https://alifei05.cfp.cn/creative/vcg/800/new/VCG41688057449.jpg',
    'https://avatars.githubusercontent.com/u/23358293?v=4',
    'ksqsf',
    'Jan 13, 2023',
    555,
    10,
    20,
    outfile='out.png'
)
