# BGRABitmap Assistant: https://chat.openai.com/g/g-FO7aiutRx-bgrabitmap-assistant
# This program extracts method signatures in BGRABitmap to help provide accurate examples

import os
import re

def gather_method_signatures(source_dir):
    print("Source directory:", source_dir)
    regexMethod = r"(?P<proc_or_fun>procedure|function)\s+(?P<class>\w+)\.(?P<method>\w+)(\((?P<parameters>[^)]*)\))?(:(?P<return_type>[^;]+))?;"
    patternMethod = re.compile(regexMethod)
    signatures = []
    classes = set()

    for root, dirs, files in os.walk(source_dir):
        for file in files:
            if file.endswith('.pas') or file.endswith('.inc'):
                print("Source file:", file)
                with open(os.path.join(root, file), 'r') as source_file:
                    content = source_file.read()
                    matches = patternMethod.finditer(content)

                    for match in matches:
                        proc_or_fun = match.group('proc_or_fun')
                        class_name = match.group('class')
                        method_name = match.group('method')
                        parameters = match.group('parameters')
                        if parameters is None: 
                            parameters = ""
                        else:
                            parameters = re.sub(r'\s', '', parameters).replace(':', ': ').replace(';', '; ')
                        return_type = match.group('return_type')
                        if return_type is None:
                            return_type = ""
                        else:
                            return_type = return_type.strip()
                        method_signature = f'{proc_or_fun} {class_name}.{method_name}({parameters})'
                        if proc_or_fun == "function":
                            method_signature += f': {return_type}'
                        signatures.append(f"{method_signature}; {{in {file}}}")
                        classes.add(class_name)
    return signatures, classes

app_directory = os.path.dirname(__file__)
source_directory = os.path.join(app_directory, '../../bgrabitmap')
signatures, classes = gather_method_signatures(source_directory)
target_file = os.path.join(app_directory, 'all.pas')
with open(target_file, "w") as file:
    for line in signatures:
        file.write(line + "\n")
target_file = os.path.join(app_directory, 'all_classes.txt')
with open(target_file, "w") as file:
    file.write(", ".join(classes))
