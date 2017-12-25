import csv, sys

def getDummyVariable(col):
    return set(col)

def transpose(mat):
    mat = [[mat[i][j] for i in range(len(mat))] for j in range(len(mat[0]))]
    return mat

if __name__ == '__main__':
    file_ = sys.argv[1]
    new_lines = []
    with open(file_, "r") as f_:
        reader = csv.reader(f_)
        lines = [line for line in reader if "?" not in line]
    print(len(lines))
    header = lines[0]
    transp = transpose(lines[1:])
    col = [1, 5, 6, 7, 8, 9, 13]
    to_use = [0, 2, 4, 10, 11, 12]
    dummy_s = []
    for i in range(len(transp)):
        if i in col:
            dummy = list(getDummyVariable(transp[i]))
            dummy_s.append(dummy)
        elif i!=3:
            dummy_s.append([header[i]])
    print(dummy_s)
    new_csv = []
    for line in lines[1:]:
        line = line[:3]+line[4:]
        new_row = []
        for i in range(len(line)):
            if len(dummy_s[i])==1:
                new_row.append(line[i])
            else:
                for el in dummy_s[i]:
                    if el in line:
                        new_row.append(1)
                    else:
                        new_row.append(0)
        new_csv.append(new_row)
    new_header = []
    for d in dummy_s:
        new_header+=d
    for i in range(len(new_header)):
        print(new_header[i], new_csv[0][i])
    for line in new_csv:
        if line[-1]=="<=50K":
            line[-1] = 1
        else:
            line[-1] = 2
    with open("./parsed2.csv", "w") as w:
        write = csv.writer(w, delimiter=",")
        write.writerow(new_header)
        for line in new_csv:
            write.writerow(line)
    #print(new_csv[0])
