# Running C# Tests

Requires [.NET SDK](https://dotnet.microsoft.com/download) (6.0+).

## Run all tests

```bash
dotnet test CSharp/Tests/Tests1/Tests1.csproj
dotnet test CSharp/Tests/Tests2/TestsZ.csproj
```

## Run a specific test project

| Project | What it covers |
|---------|----------------|
| `Tests1` | Core clipping, offsets, polytree, open paths |
| `Tests2` (TestsZ) | Z-callback / USINGZ functionality |

## Useful flags


