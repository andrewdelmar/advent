using System.Linq;

string[] lines = File.ReadAllLines("input.txt");
IEnumerable<char[]> chars = lines.Select(l => l.ToCharArray());
char[][] wordSearch = chars.ToArray();

int XSize = wordSearch.Length;
int YSize = wordSearch[0].Length;

bool validCood(int x, int y)
{
    return x >= 0 && x < XSize && y >= 0 && y < YSize;
}

bool hasXmas(int x, int y)
{
    int count = 0;
    for (int xDir = -1; xDir <= 1; xDir++)
    {
        for (int yDir = -1; yDir <= 1; yDir++)
        {
            if (xDir != 0 && yDir != 0)
            {
                int cx = x - xDir; int cy = y - yDir; int i = 0;
                char[] word = new char[] { 'M', 'A', 'S' };
                while (i < word.Length && validCood(cx, cy) && wordSearch[cx][cy] == word[i])
                {
                    cx += xDir;
                    cy += yDir;
                    i++;
                }

                if (i == word.Length)
                {
                    count++;
                }
            }
        }
    }

    return count == 2;
}

int total = 0;
for (int x = 0; x < XSize; x++)
{
    for (int y = 0; y < YSize; y++)
    {
        if (hasXmas(x, y))
        {
            total++;
        }
    }
}

Console.WriteLine("Total X-MAS: {0}", total);