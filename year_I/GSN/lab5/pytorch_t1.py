import torch

x = torch.rand(5, 3)
print(x)
x = torch.tensor([5.5, 3])
print(x)
x = x.new_ones(5, 3, dtype=torch.double)
print(x)
x = torch.rand_like(x, dtype=torch.float)
print(x)
print(x.size())

y = torch.rand(5, 3)
print(x+y)
y.add_(x)
print(y)
print(x[:,1])

a = torch.ones(5)
print(a)
b = a.numpy()
print(b)

if torch.cuda.is_available():
    device = torch.device("cuda")
    y = torch.ones_like(x, device=device)
    x = x.to(device)
    z = x + y
    print(z)
    print(z.to("cpu", torch.double))
