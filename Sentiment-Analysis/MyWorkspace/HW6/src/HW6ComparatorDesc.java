import java.util.Comparator;
import java.util.Map;

/**
 * Comparator for HW6
 * 
 * @author 
 *
 */
class HW6ComparatorDesc implements Comparator<Map.Entry<String, Double>> {
	@Override
	public int compare(Map.Entry<String, Double> o1, Map.Entry<String, Double> o2) {
		return o2.getValue().compareTo(o1.getValue());
	}
}
