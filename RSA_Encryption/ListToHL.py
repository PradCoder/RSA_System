def main():
    text = open("text.txt","r")
    out = open("out.txt","w")
    stri = text.readlines()
    biglist =[]
    for line in stri:
        for elem in line.split(" "):
            if(elem.isdigit()):
                biglist.append(int(elem))
    out.write("[")
    for i in range(len(biglist)-1):
        out.write(str(biglist[i]))
        out.write(",")
    out.write(str(biglist[len(biglist)-1]))
    out.write("]")
    out.close()
    text.close()

if __name__=="__main__":
    main()