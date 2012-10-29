using System;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;
using Microsoft.FSharp.Core;
using Brushes = System.Windows.Media.Brushes;
using Color = System.Windows.Media.Color;
using Rectangle = System.Windows.Shapes.Rectangle;
using NNCore;
using Microsoft.FSharp.Math;

namespace NumbersDemo
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private const int PointsArraySize = 20;
        private const int DrawingArraySize = 200;
        private const int RectSize = 10;
        private Polyline p;
        private readonly FSharpFunc<Matrix<double>, double> predictor = NN.buildPredictor("theta_custom.txt", trainedLayers);
        private static readonly int[] trainedLayers =  new[]{400,50,25,5};

        private const int Area = 30;
        private const double CorrectionCoeff = 1.05;
        private int iter = 0;

        public MainWindow()
        {
            InitializeComponent();
        }

        private void DrawingCanvasMouseMove(object sender, MouseEventArgs e)
        {
            if (e.LeftButton == MouseButtonState.Pressed)
            {
                if (p == null)
                {
                    p = new Polyline() {Stroke = Brushes.Black, StrokeThickness = 4};
                    drawingCanvas.Children.Add(p);
                }

                p.Points.Add(e.GetPosition(drawingCanvas));
            }
            else
            {
                p = null;
            }
        }

        private void ClearButtonClick(object sender, RoutedEventArgs e)
        {
            drawingCanvas.Children.Clear();
        }

        private void TestButtonClick(object sender, RoutedEventArgs e)
        {
            viewCanvas.Children.Clear();

            var singleDim = TransformDrawnImageToStandartView();
            DrawArray(singleDim);
            var predictedValue = predictor.Invoke( MatrixModule.ofList(NN.makeList(singleDim)));
            prediction.Content = string.Format("Prediction is {0}", predictedValue);
            iter++;
            drawingCanvas.Children.Clear();
        }

        private double[] TransformDrawnImageToStandartView()
        {
            var allPoints = drawingCanvas.Children.OfType<Polyline>().SelectMany(p => p.Points);
            var array = new int[DrawingArraySize, DrawingArraySize];
            foreach (var point in allPoints)
            {
                array[Math.Min(DrawingArraySize - 1, (int)point.X), Math.Min(DrawingArraySize - 1, (int)point.Y)] = 1;
            }

            var singleDim = new double[PointsArraySize*PointsArraySize];
            const int pointsToDrawingRatio = DrawingArraySize/PointsArraySize;
            for (int i = 0; i < PointsArraySize; i++)
            {
                for (int j = 0; j < PointsArraySize; j++)
                {
                    singleDim[i*PointsArraySize + j] = AvgInArrayArea(array, i*pointsToDrawingRatio, j*pointsToDrawingRatio);
                }
            }
            return singleDim;
        }

        private static double AvgInArrayArea(int[,] array, int x, int y)
        {
            var diff = Area / 2;
            var startX = Math.Max(0, x - diff);
            var startY = Math.Max(0, y - diff);
            var finishX = Math.Min(array.GetUpperBound(0), x + diff);
            var finishY = Math.Min(array.GetUpperBound(1), y + diff);

            var sum = 0;

            for (int i = startX; i < finishX; i++)
            {
                for (int j = startY; j < finishY; j++)
                {
                    sum += array[i, j];
                }
            }
            return sum / CorrectionCoeff;
        }

        private void DrawArray(double[] fn)
        {
            for (var i = 0; i < PointsArraySize; i++)
            {
                for (var j = 0; j < PointsArraySize; j++)
                {
                    DrawView(i * RectSize, j * RectSize, fn[PointsArraySize * i + j]);
                }
            }
            viewCanvas.InvalidateVisual();
        }

        private void DrawView(int x, int y, double color)
        {
            var grayType = (byte)(255 - color * 255);
            var rect = new Rectangle
            {
                Stroke = new SolidColorBrush(Color.FromRgb(grayType, grayType, grayType)),
                StrokeThickness = RectSize
            };
            Canvas.SetLeft(rect, x);
            Canvas.SetTop(rect, y);
            viewCanvas.Children.Add(rect);

        }

        //private static void AppendExampleToTrainSet(double[] singleDim)
        //{
        //    var line = String.Join(" ", singleDim.Select(s => s.ToString()));
        //    File.AppendAllLines("4.txt", new[] {line});
        //}
    }
}