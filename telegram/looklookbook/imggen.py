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
