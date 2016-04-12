
import glob
import eyed3
import os

for f in glob.iglob('IV/*.mp3'):
    print f

    afile = eyed3.load(f)
    newtitle = os.path.basename(f).replace('.mp3', '')
    afile.tag.title = newtitle.decode('utf-8')
    afile.tag.save()

