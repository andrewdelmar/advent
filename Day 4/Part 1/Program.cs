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

int countXmas(int x, int y)
{
    int count = 0;
    for (int xDir = -1; xDir <= 1; xDir++)
    {
        for (int yDir = -1; yDir <= 1; yDir++)
        {
            if (!(xDir == 0 && yDir == 0))
            {
                int cx = x; int cy = y; int i = 0;
                char[] word = new char[] { 'X', 'M', 'A', 'S' };
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

    return count;
}

int total = 0;
for (int x = 0; x < XSize; x++)
{
    for (int y = 0; y < YSize; y++)
    {
        total += countXmas(x, y);
    }
}

Console.WriteLine("Total XMAS: {0}", total);