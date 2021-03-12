import os

path = os.getcwd()

filelist = []
for root, dirs, files in os.walk(path + '/source'):
    for name in files :
        filelist.append(os.path.join(root, name))

strset = set([])
for filename in  filelist:
    if '.md' not in filename:
        continue

    with open(filename, "r",encoding='utf-8') as f:
        for subset in f.read():
            strset.add(subset)

str_ = ''
a = 0
with open('strdb.txt', "w") as f:
    for i in list(strset):
        str_ += i
        a += 1
    print('%d Characters in all files.'%a)
    print(str_)
    f.write(str_)