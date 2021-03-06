import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * 
 * @author
 *
 */
public class nbtrain {
	static int POS_COUNT;
	static int NEG_COUNT;

	/**
	 * @param args
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		// Maps for storing positive and negative sentiments
		HashMap<String, Integer> positiveSentiments = new HashMap<String, Integer>();
		HashMap<String, Integer> negativeSentiments = new HashMap<String, Integer>();
		// Maps for storing vocabulary
		HashMap<String, Integer> vocabularysize = new HashMap<String, Integer>();
		CalculateSentiments sentiments = new CalculateSentiments();
		// get the term and it's weight
		positiveSentiments = sentiments.getSentiment(positiveSentiments, args[0] + "\\pos");
		negativeSentiments = sentiments.getSentiment(negativeSentiments, args[0] + "\\neg");

		HashMap<String, Double> wordPosProbMap = new HashMap<String, Double>();
		HashMap<String, Double> wordNegProbMap = new HashMap<String, Double>();

		// Generate the vocabulary
		vocabularysize = sentiments.calculateVocubularySize(vocabularysize, positiveSentiments);
		vocabularysize = sentiments.calculateVocubularySize(vocabularysize, negativeSentiments);
		// remove extra space
		vocabularysize.remove("");
		// Generate positive sentiments
		positiveSentiments = sentiments.removeTerms(positiveSentiments, vocabularysize);
		// Generate negative sentiments
		negativeSentiments = sentiments.removeTerms(negativeSentiments, vocabularysize);
		vocabularysize = sentiments.removeTerms(vocabularysize, vocabularysize);

		// Calculate all the term count for positive and negative sentiments
		Iterator<String> posIt = positiveSentiments.keySet().iterator();

		while (posIt.hasNext()) {
			POS_COUNT += positiveSentiments.get(posIt.next());
		}

		Iterator<String> negIt = negativeSentiments.keySet().iterator();

		while (negIt.hasNext()) {
			NEG_COUNT += negativeSentiments.get(negIt.next());
		}

		// Calculate the probability of the positive and negative terms
		wordPosProbMap = sentiments.calculateProbability(positiveSentiments, POS_COUNT, vocabularysize);
		wordNegProbMap = sentiments.calculateProbability(negativeSentiments, NEG_COUNT, vocabularysize);

		// writer model file
		sentiments.writeModel(vocabularysize, wordPosProbMap, wordNegProbMap, args[1]);
		LinkedHashMap<String, Double> negToPosProbMap = new LinkedHashMap<String, Double>();
		LinkedHashMap<String, Double> posToNegProbMap = new LinkedHashMap<String, Double>();

		// Generate the log ratio of positive to negative and negative to positive
		Iterator<String> finalIt = vocabularysize.keySet().iterator();
		while (finalIt.hasNext()) {
			String term = finalIt.next();
			Double pos = wordPosProbMap.get(term);
			Double neg = wordNegProbMap.get(term);
			if (neg != 0) {
				Double posToNeg = Math.log(pos / neg);
				posToNegProbMap.put(term, posToNeg);
			}
			if (pos != 0) {
				Double negToPos = Math.log(neg / pos);
				negToPosProbMap.put(term, negToPos);
			}
		}
		
		// Sort the maps
		LinkedHashMap<String, Double> sortedPosToNegProbMap = sortMap(posToNegProbMap, new HW6ComparatorDesc());
		LinkedHashMap<String, Double> sortedNegToPosProbMap = sortMap(negToPosProbMap, new HW6ComparatorDesc());

		// write the results
		BufferedWriter writer = new BufferedWriter(new FileWriter("resources\\results\\top20ratio.txt"));
		int cnt = 0;
		writer.write("----------------------------------Positive To Negative------------------------------------\n");
		finalIt = sortedPosToNegProbMap.keySet().iterator();
		while (finalIt.hasNext()) {
			if (cnt == 20) {
				break;
			}
			String term = finalIt.next();
			writer.write((cnt + 1) + ". " + term + "\n");
			cnt++;
		}

		cnt = 0;
		writer.write("---------------------------------Negative To Positive--------------------------------------\n");
		finalIt = sortedNegToPosProbMap.keySet().iterator();
		while (finalIt.hasNext()) {
			if (cnt == 20) {
				break;
			}
			String term = finalIt.next();
			writer.write((cnt + 1) + ". " + term + "\n");
			cnt++;
		}

		// close the writer
		writer.close();
	}

	/**
	 * Sort the Map
	 * 
	 * @param probMap the probability map
	 * @param c the comparator
	 * @return the sorted map
	 */
	public static LinkedHashMap<String, Double> sortMap(HashMap<String, Double> probMap,
			Comparator<Map.Entry<String, Double>> c) {
		// sort the terms by probability
		List<Map.Entry<String, Double>> entries = new LinkedList<Map.Entry<String, Double>>(probMap.entrySet());
		Collections.sort(entries, c);

		LinkedHashMap<String, Double> sortedMap = new LinkedHashMap<String, Double>();
		// add all entries into sorted linked hashmap
		for (Map.Entry<String, Double> entry : entries) {
			sortedMap.put(entry.getKey(), entry.getValue());
		}

		return sortedMap;
	}
}
