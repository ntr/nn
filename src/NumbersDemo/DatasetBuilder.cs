using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace NumbersDemo
{
    //composes train dataset
    public class DatasetComposer
    {
        private const int ExamplesCount = 50;

        public static void GatherFilesIntoSingle(string[] args)
        {
            const string dataFolder = "rawdata";
            File.WriteAllLines("Y_custom.txt", Enumerable.Range(0, 4).SelectMany(i => Enumerable.Repeat(i.ToString(), ExamplesCount)));
            File.WriteAllLines("X_custom.txt", Directory.GetFiles(dataFolder, "*.txt").SelectMany(GetValidLines));
        }

        private static bool IsLineValid(string line)
        {
            return line.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries).Any(part => Math.Abs(Double.Parse(part) - 0) > 1E-6);
        }

        private static IEnumerable<string> GetValidLines(string path)
        {
            var lines = File.ReadAllLines(path).Where(IsLineValid).Take(50).ToList();
            if (lines.Count < ExamplesCount)
            {
                throw new Exception(string.Format("not enough valid examples at {0} ", path));
            }
            return lines;
        } 
    }
}