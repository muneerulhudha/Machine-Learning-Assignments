import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

class Tree {
	public Tree leftNode;
	public Tree rightNode;
	public Tree parent;
	public String featureName;
	public double entropy;
	
	public Tree(){
		featureName= "";
		leftNode=null;
		rightNode=null;
	}
}

class DecisionTree {
	static ArrayList<String> featureNames;

	Tree rootNode = null;

	Tree createRoot() {
		rootNode = new Tree();
		return rootNode;
	}
	
	boolean isAllElementsSame(ArrayList<String> array) {
		if (array.size() == 1) {
			return true;
		} else {
			String first = array.get(0);
			for (String element : array) {
				if (!element.equalsIgnoreCase(first)) {
					return false;
				}
			}
			return true;
		}
	}
	
	private List<String> filterFeatures(List<Integer> columnIndices,
			List<String> keySet) {
		List<String> features = new ArrayList<String>();
		for (int i = 0; i < columnIndices.size(); i++) {
			features.add(keySet.get(columnIndices.get(i)));
		}
		return features;
	}
	
	public Tree buildTree(List<Integer> rowIndices, List<Integer> columnIndices, Tree tree) throws IOException{
//		int noOfRows = Main.featureValues.get("Class").size();
//		String[] classValues = new String[rowIndices.size()];
		ArrayList<String> newValues = new ArrayList<String>();
		
		for (int i : rowIndices) {
			newValues.add(Main.featureValues.get("Class").get(i));
		}
		
		if (columnIndices.size() == 1) {
			int zeroCount = 0, oneCount = 0;
			for (int i = 0; i < newValues.size(); i++) {
				if (newValues.get(i).equals("0")) {
					zeroCount++;
				} else if (newValues.get(i).equals("1")) {
					oneCount++;
				}

			}
			if (oneCount < zeroCount) {
				tree.featureName = "1";
			} else {
				tree.featureName = "0";
			}
			tree.leftNode = null;
			tree.rightNode = null;
			return tree;
		}
		
		if (newValues.size() == 0) {
			tree.featureName = "";
			tree.leftNode = null;
			tree.rightNode = null;
			return tree;
		}

		if (isAllElementsSame(newValues)) {

			tree.featureName = newValues.get(0);
			tree.leftNode = null;
			tree.rightNode = null;
			return tree;
		}
		
		List<String> updatedFeatures = filterFeatures(columnIndices,
				Main.featureList);
		for (String key : updatedFeatures) {
			if (key.equalsIgnoreCase("Class")) {
				Main.currentEntropy(
						Main.probZeroandOne(key, rowIndices, columnIndices));
			}

			else {
				Main.currentEntropyNonClass(
						key,
						Main.probZeroandOne(key, rowIndices, columnIndices),
						rowIndices, columnIndices);

			}

		}
		HashMap<Integer, String> best = Main.BestNodeInfoGain(columnIndices,
				rowIndices);
		Object[] key = (Object[]) best.keySet().toArray();
		int indexOfAttribute = (Integer) (key[0]);
		tree.featureName = best.get(indexOfAttribute);
		System.out.println("Best attribute " + best.get(indexOfAttribute));
		Tree leftChild = new Tree();
		tree.leftNode = leftChild;
		Tree rightChild = new Tree();
		tree.rightNode = rightChild;

		List<Integer> rowIndicesOne = new ArrayList<Integer>();
		List<Integer> rowIndicesZero = new ArrayList<Integer>();
		List<String> s = Main.featureValues.get(best.get(indexOfAttribute));
		
		
		List<String> slimit = new ArrayList<String>();
		for (int i = 0; i < rowIndices.size(); i++) {
			slimit.add(s.get(rowIndices.get(i)));
		}
		
		s = slimit;
		
		for (int i = 0; i < rowIndices.size(); i++) {
			if (s.get(i).equalsIgnoreCase("0")) {
				rowIndicesZero.add(rowIndices.get(i));
			} else {
				rowIndicesOne.add(rowIndices.get(i));
			}
		}

		// columnIndices.remove(indexOfAttribute);
		
		int toRemove = 0;
		
		for (int i = 0; i < columnIndices.size(); i++) {
			if (indexOfAttribute == columnIndices.get(i)) {
				toRemove = i;
			}
		}
		columnIndices.remove(toRemove);

		buildTree(rowIndicesZero, columnIndices, tree.leftNode);
		buildTree(rowIndicesOne, columnIndices, tree.rightNode);
		return tree;
	}
	
	void printTree(Tree tree) {
		printTree(tree, false, 0);
	}

	String getIndentForLevel(int level) {
		String indent = "";
		for (int i = 1; i <= level; i++) {
			indent = indent + " | ";
		}
		return indent;
	}

	public void printTree(Tree node, boolean skipLeft, int level) {
		if (node == null) {
			return;
		}
		if (!skipLeft) {
			if (node.leftNode == null) {
				System.out.println(node.featureName);
				skipLeft = true;
			} else {
				System.out.println();
				System.out.print(getIndentForLevel(level) + node.featureName
						+ " = 0 : ");
				level = level + 1;
			}
		} else {
			if (node.rightNode == null) {
				System.out.print(node.featureName);
			}
		}
		printTree(node.leftNode, skipLeft, level);
		if (node.rightNode != null) {
			System.out.print(getIndentForLevel(level - 1) + node.featureName
					+ " = 1 : ");
		}
		printTree(node.rightNode, skipLeft, level);
	}

	public void printRootToLeafPaths(Tree node, List<Tree> nodelist) {
		if (node != null) {
			nodelist.add(node);
			if (node.leftNode != null) {
				printRootToLeafPaths(node.leftNode, nodelist);
			}
			if (node.rightNode != null) {
				printRootToLeafPaths(node.rightNode, nodelist);
			} else if (node.leftNode == null && node.rightNode == null) {

				for (int i = 0; i < nodelist.size(); i++) {
					if (i == nodelist.size() - 1)
						System.out.print(nodelist.get(i).featureName);
					else
						System.out.print(nodelist.get(i).featureName + "->");
				}
				System.out.println();
			}
			nodelist.remove(node);
		}
	}

	public float treeAccuracy(HashMap<String, ArrayList<String>> dataRowMap,Tree root)
	{
		int sampleDataCount = 0;
		int positiveCount = 0;
		sampleDataCount = dataRowMap.get("Class").size();
		
		for(int i=0;i<sampleDataCount;i++)
		{
			Tree temp=root;
			HashMap<String,String> dataRow = new HashMap<String,String>();
			dataRow = getDataLine(dataRowMap,i);
			if(dataRowMap.get("Class").get(i).equals(predict(dataRow,i,temp)))
				positiveCount++;
		}
		System.out.println("positive count " + positiveCount + " sample data count " + sampleDataCount);
		return ((float)positiveCount/(float)sampleDataCount);
	}

	private HashMap<String, String> getDataLine(
			HashMap<String, ArrayList<String>> data, int row) {
		HashMap<String, String> DataLine = new HashMap<String, String>();
		for(String attribute : data.keySet())
		{
			DataLine.put(attribute, data.get(attribute).get(row));	
		}
		return DataLine;
	}

	private String predict(HashMap<String, String> data,int row,Tree root) {
		try
		{
		String key = root.featureName;
		if (key.equals("0"))
			return "0";
		if (key.equals("1"))
			return "1";
		if(data.get(key).equals("0"))
			return predict(data,row,root.leftNode);
		else
			return predict(data,row,root.rightNode);
		}
		catch(Exception e)
		{
//			System.out.println("Exception caught for line number : "+row);
			return null;
		}
	}
}

public class Main {
	static int count = 0;
	static int noOfRows = 0, noOfZeroRows = 0, noOfOneRows = 0;
	static String training_set;
	static String validation_set;
	static String test_set;
	static String to_print;
	static int lvalue;
	static int kvalue;
	
	public static String[] columnNames;
	public static List<String> featureList = new ArrayList<String>();
	public static HashMap<String, ArrayList<String>> featureValues = new HashMap<String, ArrayList<String>>();
	
	private static HashMap<String, ArrayList<String>> csvToHashMap(String filename) throws IOException {
		BufferedReader br = new BufferedReader(new FileReader(filename));
		String line = null;
		
		line = br.readLine();
		columnNames = line.split(",");
		
		for (int i = 0; i < columnNames.length; i++) {
			featureList.add(columnNames[i]);
		}
		count++;
		for (String string : columnNames) {
			featureValues.put(string, new ArrayList<String>());
		}
		
		while ((line = br.readLine()) != null) {
			count++;

			String str[] = line.split(",");
			for (int i = 0; i < str.length; i++) {
				featureValues.get(columnNames[i]).add(str[i]);
			}
		}
		br.close();
		return featureValues;
	}
	
	public static double probZeroandOne(String key, List<Integer> rowIndices,
			List<Integer> columnIndices) {
		// Reduce number of rows for a column and not the columns itself for
		// each subset
		ArrayList<String> classValues = featureValues.get(key);
		int totalZeroCount = 0, totalOneCount = 0;
		for (int index : rowIndices) {
			if (classValues.get(index).equals("0")) {
				totalZeroCount++;
			} else if (classValues.get(index).equals("1")) {
				totalOneCount++;
			}
		}
		int totalCount = totalZeroCount + totalOneCount;
		double probZero = (double) totalZeroCount / (double) totalCount;
		// double probOne = (double) totalOneCount / (double) totalCount;
		return (probZero); // probOne=1-probZero

	}
	
	public static double currentEntropy( Double probZero) {
		double currentEntropy = 0;
		double probOne = Math.abs(1 - probZero);
		if (probZero == 0) {
			currentEntropy = (float) -((probOne) * (Math.log10(probOne) / Math
					.log10(2.0d)));
		} else if (probOne == 0) {
			currentEntropy = (float) (-(probZero) * (Math.log10(probZero) / Math
					.log10(2.0d)));
		} else if (probZero == 0 && probOne == 0) {
			currentEntropy = 0.0f;
		} else {
			// currentEntropy = -((probOne) * (Math.log10(probOne) ))
			// -((probZero) * (Math.log10(probZero))) ;
			currentEntropy = (-probZero
					* (Math.log10(probZero) / Math.log10(2.0d)) - (probOne)
					* (Math.log10(probOne) / Math.log10(2.0d)));
		}
		return currentEntropy;

	}
	
	public static double currentEntropyNonClass(String key, Double probZero,
			List<Integer> rowIndices, List<Integer> columnIndices) {
		// classValues holds the values corresponding to attribute names
		double probOne = Math.abs(1 - probZero);

		// Logic to get parameters for the individual entropies

		ArrayList<String> classValues = featureValues.get(key);
		ArrayList<String> toCompare = featureValues.get("Class");
		int ZeroPositiveCount = 0, OnePositiveCount = 0, ZeroNegativeCount = 0, OneNegativeCount = 0;
		// Logic change: Compare two array lists to avoid n^2 comparisons with
		// redundancies
		for (int index : rowIndices) {
			if (classValues.get(index).equals("0")
					&& toCompare.get(index).equals("0")) {
				ZeroNegativeCount++;
			} else if (classValues.get(index).equals("0")
					&& toCompare.get(index).equals("1")) {
				ZeroPositiveCount++;
			} else if (classValues.get(index).equals("1")
					&& toCompare.get(index).equals("0")) {
				OneNegativeCount++;
			} else if (classValues.get(index).equals("1")
					&& toCompare.get(index).equals("1")) {
				OnePositiveCount++;
			}
		}
		// int totalCount = ZeroNegativeCount + ZeroPositiveCount
		// + OneNegativeCount + OnePositiveCount;
		int totalZeroCount = ZeroNegativeCount + ZeroPositiveCount;
		int totalOneCount = OneNegativeCount + OnePositiveCount;
		// Modify this part
		double Entropy1 = 0;

		if (totalZeroCount != 0)
			Entropy1 = Entropy1
					+ ((probZero) * (currentEntropy(
							
							((double) ZeroNegativeCount / (double) totalZeroCount))));
		if (totalOneCount != 0)
			Entropy1 = Entropy1
					+ ((probOne) * (currentEntropy(
							
							((double) OneNegativeCount / (double) totalOneCount))));

		// Entropy1 = ((probZero) * (currentEntropy(key,
		// ((double) ZeroNegativeCount / (double) totalZeroCount))))
		// + ((probOne) * (currentEntropy(key,
		// ((double) OneNegativeCount / (double) totalOneCount))));

		return Entropy1;

	}
	
	public static double InformationGain(String key, Double OverallEntropy,
			Double AttributeEntropy) {

		double infoGain;
		infoGain = Math.abs(OverallEntropy - AttributeEntropy);
		System.out.println("Information Gain on "+ key + " " + infoGain);
		return infoGain;
	}
	
	public static HashMap<Integer, String> BestNodeInfoGain(
			List<Integer> columnIndices, List<Integer> rowIndices)
			throws IOException {

		ArrayList<String> newKeySet = new ArrayList<String>();
		// Now get the keys for the indices in columnIndices list
		Main.csvToHashMap(training_set);
		for (int i : columnIndices) {
			newKeySet.add(Main.columnNames[i]);

		}

		int index = 0;
		double maxGain = -999;
		String keyMax = null;
		for (String i : newKeySet) {
			if (!i.equalsIgnoreCase("Class")) {
				double temp = InformationGain(
						i,
						currentEntropy(
								probZeroandOne(i, rowIndices, columnIndices)),
						currentEntropyNonClass(i,
								probZeroandOne(i, rowIndices, columnIndices),
								rowIndices, columnIndices));

				if ((temp > maxGain)) {
					maxGain = temp;
					keyMax = i;
				}
			}
		}
		// return index of the key with the max gain
		Main.csvToHashMap(training_set);
		for (int i = 0; i < Main.columnNames.length; i++) {
			if (Main.columnNames[i].equalsIgnoreCase(keyMax)) {
				index = i;
			}
		}
		System.out.println("Best Index = " + index);
		HashMap<Integer, String> best = new HashMap<Integer, String>();
		best.put(index, keyMax);
		return best;

	}
	
	public static void main(String[] args) throws IOException {
		training_set = args[2];
		validation_set = args[3];
		test_set = args[4];
		to_print = args[5];
		String l = args[0];
		String k = args[1];
		
		lvalue = Integer.parseInt(l);
		kvalue = Integer.parseInt(k);
		
		csvToHashMap(training_set);
		
		List<Integer> rowIndices = new ArrayList<Integer>();
		List<Integer> columnIndices = new ArrayList<Integer>();
		
		noOfRows = featureValues.get("Class").size();
		for (int i = 0; i < noOfRows; i++) {
			rowIndices.add(i);
		}
		
		int noOfColumns = featureValues.keySet().size();
		for (int i = 0; i < noOfColumns; i++) {
			columnIndices.add(i);
		}

		for (String i : featureValues.get("Class")) {
			if (i.equalsIgnoreCase("0")) {
				noOfZeroRows++;
			} else if (i.equalsIgnoreCase("1")) {
				noOfOneRows++;
			}
		}
		
		DecisionTree dt = new DecisionTree();
		
		Tree tree = dt.createRoot();
		Tree tree1 = dt.buildTree(rowIndices, columnIndices, tree);
		if(to_print.equals("yes"))
			dt.printTree(tree1);

		//Tree prunedTree = dt.pruneTree(lvalue, kvalue, tree1);
		dt.printRootToLeafPaths(tree1, new ArrayList<Tree>());
		HashMap<String, ArrayList<String>> testData = csvToHashMap(test_set);
		System.out.println("Tree Accuracy is: "+dt.treeAccuracy(testData, tree1));
		
	}
}
