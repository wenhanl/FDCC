/**
 * 
 */

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;


/**
 * @author Nicolas_Yu
 *
 */
public class Sweep_subgraph {
	private String seed;
	private String graphPathStr;
	private double conductance;
	private Set<String> S;
	private Set<String> starS;
	//private Map<String, Double> pr;
	private List<Map.Entry<String, Double>> sortedPrs;
	
	public Sweep_subgraph(String seedStr, String graphPathStr, Map<String, Double> p) {
		this.seed = seedStr;
		this.graphPathStr = graphPathStr;
		//this.pr = p;
		this.conductance = Double.MAX_VALUE;
		this.S = new HashSet<String>();
		this.S.add(seed);
		this.starS = new HashSet<String>(this.S);
		
		this.sortedPrs = new ArrayList<Map.Entry<String,Double>>(p.entrySet());
		Collections.sort(sortedPrs, new Comparator<Map.Entry<String, Double>>() {

			@Override
			public int compare(Entry<String, Double> o1,
					Entry<String, Double> o2) {
				// TODO Auto-generated method stub
				return -o1.getValue().compareTo(o2.getValue());
			}
		});		
	}
	
	private double getConductance(Set<String> s) throws IOException {
		double conductance = 0.0;
		
		double boundary = 0.0, volumn = 0.0;
		BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(this.graphPathStr)));
		String line = null;
		
		while ((line = br.readLine()) != null) {
			String[] pages = line.split("\t");
			int length = pages.length;
			int outDegree = length - 1;
			String uKey = pages[0];
			
			if (!s.contains(uKey)) {
				continue;
			}
			
			for (int i = 1; i <= outDegree; i++) {
				if (!s.contains(pages[i])) boundary++;
			}
			volumn = volumn + outDegree;
		}
		br.close();
		
		conductance = boundary / volumn;
		return conductance;
	}
	
	
	public double getMinConductance() {
		return this.conductance;
	}
	
	public Set<String> getSubGraph() {
		return Collections.unmodifiableSet(this.starS);
	}
	
	public void sweep() throws IOException {
		for (Map.Entry<String, Double> prEntry: this.sortedPrs) {
			this.S.add(prEntry.getKey());
			
			double currentConductance = this.getConductance(this.S);
			
			if (currentConductance < this.conductance) {
				this.starS = new HashSet<String>(this.S);
				this.conductance = currentConductance;
			}
		}
	}	
}
