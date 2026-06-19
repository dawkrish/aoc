import hashlib

# 1. Initialize the MD5 object


def bar():
    print("Starting...")
    pad = "iwrupvqb"
    cnt = 1
    current_hex = None
    while True:
        x = pad + str(cnt)
        ans = foo(x)
        valid = ans.startswith('000000')
        if valid:
            current_hex = ans
            break
        else:
            cnt +=1
    print(cnt)
    print(f"The answer is {cnt}, because md5({pad + str(cnt)}) = {current_hex}")


def foo(x):
    md5_hash = hashlib.md5()
    md5_hash.update(x.encode('utf-8'))

    # 3. Get the 32-character hexadecimal string
    hex_output = md5_hash.hexdigest()
    return hex_output


bar()
