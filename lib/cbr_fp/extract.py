import subprocess
import json
def load_dataset(n):
    # Run the Python program and capture its output
    python_process = subprocess.Popen(['python', 'load.py', str(n)], stdout=subprocess.PIPE)

    # Get the output from the Python program
    python_output = python_process.communicate()[0].decode('utf-8').strip()

    # Split the string into lines and exclude the first two lines
    lines = python_output.splitlines()[2:]

    # Join the remaining lines back into a string
    python_output = '\n'.join(lines)

    # Run the command and pass the Python output as input
    command = 'elm-format --stdin --json'
    shell_process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE, shell=True)
    shell_output, _ = shell_process.communicate(input=python_output.encode('utf-8'))
    shell_output = shell_output.decode('utf-8').strip()
    data_dict = json.loads(shell_output)
    return data_dict

text = ""
for i in range(20):
    data_dict = load_dataset(i)
    body = data_dict['body']
    for block in body:
        if block.get('tag') == 'Definition' and block['expression']['tag'] == 'CaseExpression':
            if block['expression']['subject']['tag'] == 'VariableReference':
                v = block['expression']['subject']['name']
                for param in block['parameters']:
                    type = param['type']
                    pat = param['pattern']
                    if pat.get('name') == v:
                        if type['tag'] == 'TypeReference':
                            if type['name'] == 'Maybe' or type['name'] == 'Result' or type['name'] == 'List':
                                text += str(block)
                                text += '\n'

with open("examples.txt", "w") as file:
    file.write(text)