def go_right(word):
    for x in word:
        try:
            return int(x)
        except:
            continue


def go_left(word):
    return go_right(reversed(word))


def advent_1_1(fname):
    return (
        int(f'{go_right(word)}{go_left(word)}')
        for word in read_file(fname)
    )

def read_file(filename):
    file1 = open(filename, 'r')
    for line in file1.readlines():
        yield line.strip()
    file1.close()


if __name__ == "__main__":
    values = advent_1_1('problem_1/input.txt')
    for value in values:
        print(value)