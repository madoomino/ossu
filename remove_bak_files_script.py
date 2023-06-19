import os
for root, dirs, files in os.walk("."):
    for file in files:
        if file.endswith(".bak"):
            os.remove(os.path.join(root, file))
