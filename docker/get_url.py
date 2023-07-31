import urllib.request, json, sys

# Prints the URL of the desired Zig version using argv
with urllib.request.urlopen("https://ziglang.org/download/index.json") as url:
    data = json.load(url)
    print(data[sys.argv[1]][sys.argv[2]]["tarball"])
