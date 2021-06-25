import os
from termcolor import cprint

def get_confirm(keyboard_input:str):
    if keyboard_input.strip() == 'Y' or keyboard_input == 'y' or keyboard_input == 'Yes' or keyboard_input == 'yes':
        return True
    else:
        return False
    

def main():
    cprint("Hello, this is the module of adding new (empty) folders for new lessons. Press Y/N to continue...\n",'cyan')
    while get_confirm(input()):
        cprint("Enter name...\n",'cyan')
        name = input().strip().lower()
        if os.path.exists(os.path.abspath('.') + '/' + name):
            cprint("Lesson exists!",'red')
        else:
            os.mkdir(os.path.abspath('.') + '/' + name)
        cprint("Add more?\n",'cyan')
    cprint("Terminating...",'red')
if __name__ == '__main__':
    main()