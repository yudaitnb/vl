from z3 import *
import re
import subprocess
from z3 import BitVec, BitVecVal
from math import ceil, log2
import time
from z3 import parse_smt2_file, sat, Solver

# 新しいSolverのサブクラス
class SMTLib2OutputSolver(Solver):
    def __init__(self):
        super().__init__()
        self.smtlib2_script = ""

    def add(self, constraint):
        self.smtlib2_script += f"(assert {constraint})\n"
        super().add(constraint)

    def get_smtlib2_script(self):
        return self.smtlib2_script

def parse_input(file_name):
    with open(file_name, 'r') as f:
        lines = f.readlines()
    
    modules_versions = {}
    constraints = []
    label_set = set()  # 追加: ラベルのセットを保持
    is_constraint = False
    for line in lines:
        line = line.strip()
        if not line:  # skip empty lines
            continue
        if line.startswith("#"):
            if "Constraints" in line:
                is_constraint = True
            continue
        if is_constraint:
            constraints.append(line)
            # Extracting the label names/numbers
            label_matches = re.findall(r'a(\d+)', line)
            label_set.update(label_matches)  # 更新: ラベルをセットに追加
        else:
            if ":" in line:
                module, vers = line.split(":")
                versions_list = [v.strip() for v in vers.split(",")]
                modules_versions[module] = versions_list
    return modules_versions, constraints, label_set  # 更新: ラベルのセットも返す

def find_deepest_nested(s):
    max_depth = 0
    current_depth = 0
    max_depth_start = None
    start = None
    for i, c in enumerate(s):
        if c == '(':
            if current_depth == 0:
                start = i
            current_depth += 1
            if current_depth > max_depth:
                max_depth = current_depth
                max_depth_start = start
        elif c == ')':
            current_depth -= 1
            if current_depth < 0:
                raise ValueError("Unmatched parentheses in string.")
            if current_depth == 0 and start == max_depth_start:
                return s[start:i+1]
    return None

def split_args(s):
    parts = []
    depth = 0
    start = 0
    for i, c in enumerate(s):
        if c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
        elif c == ' ' and depth == 0:
            parts.append(s[start:i])
            start = i + 1

    # 最後の部分を追加
    parts.append(s[start:])
    
    # print(f"split_args('{s}') -> {parts}")  # デバッグ情報を出力
    
    if len(parts) != 2:
        raise ValueError(f"Invalid constraint format: {s}")

    return parts[0].strip(), parts[1].strip()



def extract_innermost_parentheses(s):
    start = None
    depth = 0
    for i, c in enumerate(s):
        if c == '(':
            if depth == 0:
                start = i
            depth += 1
        elif c == ')':
            depth -= 1
            if depth == 0:
                return s[start:i+1]
    return None

def parse_constraint(cstr, modules, versions, label_set):
    return parse_inner_constraint(cstr, modules, versions, label_set)


def parse_inner_constraint(cstr, modules, versions, label_set):
    # print(f"parse_inner_constraint('{cstr}')")  # デバッグ情報を出力
    # Handling (and C C)
    if cstr.startswith("(and "):
        # print("Matched (and pattern")
        inner_string = cstr[5:-1].strip()
        left_cstr, right_cstr = split_args(inner_string)
        left_expr = parse_constraint(left_cstr, modules, versions, label_set)
        right_expr = parse_constraint(right_cstr, modules, versions, label_set)
        return And(left_expr, right_expr)

    # Handling (or C C)
    elif cstr.startswith("(or "):
        inner_string = cstr[4:-1].strip()
        left_cstr, right_cstr = split_args(inner_string)
        left_expr = parse_constraint(left_cstr, modules, versions, label_set)
        right_expr = parse_constraint(right_cstr, modules, versions, label_set)
        return Or(left_expr, right_expr)
    
    elif cstr.startswith("(a"):
        # print("Matched (a pattern")
        # Extracting the label names/numbers
        label_matches = re.findall(r'a(\d+)', cstr)

        # 不正なラベルの確認
        for match in label_matches:
            if int(match) not in [int(l) for l in label_set]:  # label_setの要素を整数に変換して比較
                raise ValueError(f"Invalid label detected: a{match}")

        # Handling (am <= an)
        if len(label_matches) == 2:
            m, n = int(label_matches[0]), int(label_matches[1])
            constraints = []
            for i in range(len(modules)):
                version_value_m = versions[modules[i]]["*"]
                version_value_n = versions[modules[i]]["*"]
                constraints.append(Or(labels[m][i] == labels[n][i], labels[n][i] == version_value_n))
            return And(*constraints)
        
        # Handling (am ≤ [{...}])
        elif len(label_matches) == 1 and "[{" in cstr and "}]" in cstr:
            m = int(label_matches[0])
            constraints = []
            mod_version_str = re.search(r'\[\{(.*?)\}\]', cstr).group(1)
            mod_version_pairs = mod_version_str.split(',')
            for pair in mod_version_pairs:
                mod, version = pair.split(':')
                mod = mod.strip()
                version = version.strip()[1:-1]  # remove brackets
                idx = modules.index(mod)
                constraints.append(labels[m][idx] == version)
            return And(*constraints)

    # 追加: 'True' または 'False' としての文字列のBoolRefを変換
    if cstr == "True":
        return True
    elif cstr == "False":
        return False

    else:
        # ... 他の制約の解析ロジック ...
        pass

    raise ValueError(f"Invalid constraint format: {cstr}")

def execute_smtlib2_and_get_result():
    # Start timing
    start_time = time.time()

    # Execute the Z3 solver using subprocess
    result = subprocess.run(['z3', '-smt2', 'smtlib2.txt'], capture_output=True, text=True).stdout
    
    # Stop timing
    end_time = time.time()

    # Calculate elapsed time
    elapsed_time = end_time - start_time
    print(f"Time taken for Z3 to solve: {elapsed_time:.4f} seconds")
    
    # Extract values from the Z3 output
    value_matches_int = re.findall(r'\(s_(\d+)_(\d+)\s+(\d+)\)', result)
    
    # Gather results
    values_int = {(int(m[0]), int(m[1])): int(m[2]) for m in value_matches_int}
    
    return values_int

def get_label_from_values(values, modules, modules_versions):
    # Convert value to label format
    labels_output = {}
    for (label_idx, module_idx), version_idx in values.items():
        if version_idx == 0:  # means "*"
            version_str = "*"
        else:
            version_str = modules_versions[modules[module_idx]][version_idx - 1]
        
        key = f"a{label_idx}"
        if key not in labels_output:
            labels_output[key] = []
        labels_output[key].append(f"{modules[module_idx]}: {version_str}")

    return labels_output

def display_version_to_int_mappings(versions):
    for module, version_map in versions.items():
        print(f"Module: {module}")
        for version, value in version_map.items():
            print(f"  Version: {version} -> Int Value: {value}")
        print()


def main():
    # Read from input.txt
    modules_versions, constraints_input, label_set = parse_input('input.txt')

    modules = list(modules_versions.keys())
    # バージョンマッピングの部分を修正
    versions = {module: {"*": 0} for module, vers in modules_versions.items()}
    for module, vers in modules_versions.items():
        for idx, version in enumerate(reversed(vers)):  # バージョンを逆順に処理
            versions[module][version] = idx + 1

    # Declare z3 variables
    global labels
    labels = {}
    max_label = max([int(l) for l in label_set]) + 1

    solver = Optimize()

    for i in range(max_label):
        labels[i] = [Int(f's_{i}_{j}') for j in range(len(modules))]
        for j, module in enumerate(modules):
            vers = list(versions[module].keys())
            solver.add(Or([labels[i][j] == versions[module][v] for v in vers]))

    # Process constraints from input
    for cstr in constraints_input:
        constraint = parse_constraint(cstr, modules, versions, label_set)
        solver.add(constraint)

    # Add optimization goal
    total_value = Sum([Sum(labels[i]) for i in labels])
    solver.minimize(total_value)

    # Convert to SMT-LIB2 format
    smt2_str = "(set-logic QF_LIA)\n"  # Setting the logic for Linear Integer Arithmetic

    # Adding declarations
    for i in labels:
        for j in range(len(modules)):
            smt2_str += f"(declare-const s_{i}_{j} Int)\n"

    # Adding assertions
    for c in solver.assertions():
        smt2_str += "(assert " + c.sexpr() + ")\n"

    # Adding the maximization objective
    all_vars = [labels[i][j].sexpr() for i in labels for j in range(len(modules))]
    joined_vars = ' '.join(all_vars)
    smt2_str += f"(minimize (+ {joined_vars}))\n"

    # Write to the SMT-LIB2 format file
    with open("smtlib2.txt", "w") as f:
        f.write(smt2_str)

        # Adding the commands in specified order
        f.write("(check-sat)\n")
        f.write("(get-objectives)\n")

        # Adding the get-value commands
        for i in labels:
            for j in range(len(modules)):
                f.write(f"(get-value (s_{i}_{j}))\n")

    values = execute_smtlib2_and_get_result()
    labels_output = get_label_from_values(values, modules, modules_versions)
    
    display_version_to_int_mappings(versions)

    for label, versions_list in labels_output.items():
        versions_str = ', '.join(versions_list)
        print(f"{label}: {versions_str}")
        

if __name__ == '__main__':
    main()
