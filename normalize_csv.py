import csv
import sys
from numpy import mean
from numpy import std

def transpose(matrix):
    matrix = [[matrix[i][j] for i in range(len(matrix))] for j in range(len(matrix[0]))]
    return matrix

if __name__ == '__main__':
    csv_file = sys.argv[1]
    with open(csv_file, "r") as read:
        lines = csv.reader(read, delimiter=",")
        lines = [line for line in lines]
    #print(lines)
    header = lines[0]
    data = lines[1:]
    print(header)
    data = transpose(data)
    new_data = []
    for col in data[:-1]:
        col = [float(val) for val in col]
        m = mean(col)
        s = std(col)
        col = [(val-m)/s for val in col]
        new_data.append(col)
    new_data.append(data[-1])
    new_data = transpose(new_data)
    new_data = [header]+new_data
    with open("parsed_normalized.csv", "w") as write_f:
        writ = csv.writer(write_f, delimiter=',')
        writ.writerows(new_data)
