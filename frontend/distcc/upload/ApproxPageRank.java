/**
 * 
 */

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author Nicolas_Yu
 *
 */
public class ApproxPageRank {
	
	private String seed;
	private String graphPathStr;
	private double ALPHA;
	private double EPSILON;
	
	private Map<String, Double> p;
	private Map<String, Double> r;
	
	public ApproxPageRank(String seedStr, String inputPath, double alpha, double epsilon) {
		this.seed = seedStr;
		this.graphPathStr = inputPath;
		this.ALPHA = alpha;
		this.EPSILON = epsilon;
		
		this.p = new HashMap<String, Double>();
		this.r = new HashMap<String, Double>();
		
		this.r.put(this.seed, 1.0);
	}
	
	public Map<String, Double> getPrMap() {
		return Collections.unmodifiableMap(this.p);
	}
	
	
	/**
	 * push operation 
	 * @param pages
	 */
	private void push(String[] pages) {
		int length = pages.length;
		String uKey = pages[0];
		
		final double Ru = this.r.get(uKey);
		
		int outDegree = length - 1;
		
		double W = 0.5;
		
		if (this.r.containsKey(uKey)) {
			this.r.remove(uKey);
		}
		
		if (this.p.containsKey(uKey)) {
			this.p.put(uKey, this.p.get(uKey) + Ru*ALPHA );
		} else {
			this.p.put(uKey, Ru*ALPHA);
		}		
		
		if (this.r.containsKey(uKey)) {
			this.r.put(uKey, this.r.get(uKey) + (W*Ru*(1-ALPHA)));
		} else {
			this.r.put(uKey, (W*Ru*(1-ALPHA)));
		}
		
		for (int i = 1; i < length; i++) {
			String vKey = pages[i];
			double vWeight = 1.0 / ( 2.0 * (double) outDegree);

			if (this.r.containsKey(vKey)) {
				this.r.put(vKey, this.r.get(vKey) + (1 - ALPHA) * Ru * vWeight);
			} else {
				this.r.put(vKey, (1 - ALPHA) * Ru * vWeight);
			}
		}
	}
	
	/**
	 * run approximated PageRank with a seed page s, and parameter ALPHA and EPSILON
	 * @throws IOException 
	 * @arrroxPR
	 */
	public void approxPR() throws IOException {
		boolean updated = true;
		
		BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(this.graphPathStr)));
		while (updated) {
			updated = false;
			
			String brLine = null;
			while ((brLine = br.readLine()) != null) {
				String[] pages = brLine.split("\t");
				String uKey = pages[0];
				
				if (this.r.containsKey(uKey)) {
					int length = pages.length;
					int outDegree = length - 1;
					double Ru = this.r.get(uKey);
					
					if (Ru/(double) outDegree <= EPSILON || outDegree == 0) {
						continue;
					}
					
					this.push(pages);	
					updated = true;
					
				}
			}
			
		}
	}
	
//	public void approxPR() throws IOException {
//		boolean updated = true;
//		while (updated) {
//			updated = false;
//			Set<String> rKeys = new HashSet<String>(this.r.keySet());
//			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(this.graphPathStr)));
//			for (String rKey : rKeys) {
//				double Ru = this.r.get(rKey);
//				
//				String brLine = null;
//				boolean notFound = true;
//				while ((brLine = br.readLine()) != null) {
//					String[] pages = brLine.split("\t");
//					String uKey = pages[0];
//
//					if (uKey.compareTo(rKey) == 0) {
//						int length = pages.length;
//						int outDegree = length - 1;
//						
//						if (Ru / (double) outDegree <= EPSILON || outDegree == 0) continue;
//						
//						this.push(pages);
//						
//						updated = true;
//						notFound = false;
//						break;
//					}
//				}
//				
//				if (notFound) {
//					this.push(new String[] {rKey});
//				}		
//			}
//			br.close();
//		}
//	}
	
	public static void main(String[] args) throws IOException {
		ApproxPageRank APR = new ApproxPageRank(args[1], args[0], 
				Double.parseDouble(args[2]), Double.parseDouble(args[3]));
		APR.approxPR();
		Map<String, Double> prsMap = APR.getPrMap();
		
		Sweep_subgraph sweepGraph = new Sweep_subgraph(args[1], args[0], prsMap);
		sweepGraph.sweep();
		
		Set<String> result = sweepGraph.getSubGraph();
		for (String a : result) {
			System.out.println(a + "\t" + prsMap.get(a));
		}
	    
	}
	
}
