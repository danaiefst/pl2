import requests
import sys
from bs4 import BeautifulSoup

def factorial(start, end, mod):
    acc = 1
    for i in range(start, end + 1):
        acc = (acc * i) % mod
    return acc

def comb_mod(n, k, acc):
    max = k
    min = n - k
    if k < n - k:
      max = n - k
      min = k
    fact_nom = factorial(max + 1, n, acc)
    fact_denom = factorial(2, min, acc)
    inv_denom = find_inv(fact_denom, acc)
    ret = (fact_nom * inv_denom) % acc
    if ret < 0:
        ret += acc
    return ret

def find_inv(a, p):
    s, old_s = 0, 1
    r, old_r = p, a
    while r != 0:
      quotient = old_r // r
      r, old_r = old_r - quotient * r, r
      s, old_s = old_s - quotient * s, s
    return old_s

"""url = sys.argv[1]
s = requests.Session()
r = s.get(url)

while(True):
    soup = BeautifulSoup(r.content, 'html.parser')
    print(soup.find(class_='question').get_text())
    n = int(soup.find("span", attrs={"id":"N"}).get_text())
    k = int(soup.find("span", attrs={"id":"K"}).get_text())
    p = int(soup.find("span", attrs={"id":"P"}).get_text())
    submit = soup.find("input", attrs={"class":"button"})
    combmod = comb_mod(n, k, p)
    r = s.post(url, data = {'answer': combmod, submit['name'] : submit['value']})
    soup = BeautifulSoup(r.content, 'html.parser')
    submit = soup.find("input", attrs={"class":"button"})
    resp = ""
    for i in soup.find("span", attrs={"id":"P"}).find_all_next("p"):
        resp += i.get_text()
        resp += "\n"
    print(resp)
    if "second" in r.content.decode():
        break
    r = s.post(url, data = {submit['name'] : submit['value']})"""
