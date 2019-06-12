import java.util.Queue;
import java.util.ArrayList;
import java.util.TreeMap;
import java.io.*;
class GN {
	int[] noOfShortestPaths;
	int[] level;
	ArrayList<ArrayList<Integer>> predecessor;
	int[][] mat, newMat;
	float[][] bet;
	int vertices;
	void girvanNewman() {// a  b  c  d  e  f  g
		mat = readMatrix();
		vertices = mat.length;
		bet = new float[vertices][vertices];
		for(int i=0; i<vertices; i++) {
			level = new int[vertices];
			for(int j=0; j<vertices; j++) level[j] = vertices;
			level[i] = 0;
			predecessor = new ArrayList<ArrayList<Integer>>();
			for(int j=0; j<vertices; j++) predecessor.add(new ArrayList<Integer>());
			bfs(i);
			//System.out.print("\n\nNode level from "+((char) (65+i))+":\n");
			//for(int j=0; j<vertices; j++) System.out.print(((char) (65+j))+": "+level[j]+"\t");
			newMat = new int[vertices][vertices];

			//System.out.println("\n\nAdjacency matrix with only Shortest Paths:");
			for(int j=0; j<vertices; j++) {
				ArrayList<Integer> al = predecessor.get(j);
				for(int k: al) newMat[k][j] = newMat[j][k] = 1;
			}

			/*for(int j=0; j<vertices; j++) {
				for(int k=0; k<vertices; k++)
					System.out.print(newMat[j][k]+" ");
				System.out.println();
			}*/

			noOfShortestPaths = new int[vertices];
			bfsForStep2(i);
			/*System.out.print("\n\nNo. of Shortest Paths passing through each vertex:\n");
			for(int j=0; j<vertices; j++) System.out.print(((char) (65+j))+": "+noOfShortestPaths[j]+"\t");
*/
			TreeMap<Integer, ArrayList<Integer>> map = new TreeMap<Integer, ArrayList<Integer>>();
			for(int j=0; j<vertices; j++) {
				if(!map.containsKey(level[j])) map.put(level[j], new ArrayList<Integer>());
				map.get(level[j]).add(j);
			}

			float[] nodeCredit = new float[vertices];
			float[][] currBet = new float[vertices][vertices];
			for(int clevel: map.descendingKeySet()) {
				ArrayList<Integer> al = map.get(clevel);
				for(int j: al) {
					nodeCredit[j] = 1.0f;
					for(int k=0; k<vertices; k++)
						if(newMat[j][k]==1 && level[j]<level[k]) nodeCredit[j] += currBet[j][k];
					int sum_pi = 0;
					for(int k=0; k<vertices; k++)
						if(newMat[k][j]==1 && level[j]>level[k]) sum_pi += noOfShortestPaths[k];
					for(int k=0; k<vertices; k++) 
						if(newMat[k][j]==1 && level[j]>level[k]) 
							currBet[k][j] = sum_pi==0.0f?nodeCredit[j]:((float) (nodeCredit[j]*noOfShortestPaths[k]/sum_pi));
				}
			}

			/*System.out.print("\n\nNode Credit Values:\n");
			for(int j=0; j<vertices; j++) System.out.print(((char) (65+j))+": "+nodeCredit[j]+"\t");
*/
			//System.out.println("\n\nBetweenness Values: ");
			for(int j=0; j<vertices; j++) {
				for(int k=0; k<vertices; k++) {
					//System.out.print(currBet[j][k]+" ");
					bet[j][k] += currBet[j][k];
				}
				//System.out.println();	
			}
		}

		//System.out.println("\n\nFinal Betweenness Values: ");
		for(int j=0; j<vertices; j++) {
			for(int k=0; k<vertices; k++) {
				bet[j][k] = (float) (bet[j][k]);
				//System.out.print(bet[j][k]+" ");
			}
			System.out.println();	
		}

		try { 
        	// create FileWriter object with file as parameter 
			PrintWriter pw = new PrintWriter(new File("/Users/sejalpawar/Documents/matrix.csv")); 
			int[][] finalMat = new int[vertices][vertices];
			System.out.println("\n\nEliminating edges whose Betweenness > Threshold: ");
			for(int j=0; j<vertices; j++) {
				StringBuilder line = new StringBuilder("");
				for(int k=0; k<vertices; k++) {
					if(bet[j][k]<=3.0f && mat[j][k]==1) finalMat[j][k] = 1;
					//System.out.print(finalMat[j][k]+" ");
					line.append(finalMat[j][k]+((k!=vertices-1)?",":""));
				}
				//System.out.println();	
				pw.write(line.toString()+"\n");
			}
			pw.close();
		}
		catch (IOException e) { 
        	// TODO Auto-generated catch block 
        	e.printStackTrace(); 
    	}
	}
	void bfs(int src) {
		boolean[] visited = new boolean[vertices];
		Queue<Integer> queue = new java.util.LinkedList<Integer>();
		queue.add(src);
		visited[src] = true;
		queue.add(-1);
		while(!queue.isEmpty()) {
			int current = queue.remove();
			if(current==-1) {
				if(!queue.isEmpty()) queue.add(-1);
				continue;
			}
			for(int i=0; i<vertices; i++) {
				if(mat[current][i]==1) {
					if(!visited[i]) {
						queue.add(i);
						visited[i] = true;
						predecessor.get(i).add(current);
					}
					if(level[i]>(level[current]+1)) {
						level[i] = level[current]+1;
						predecessor.get(i).clear();
						predecessor.get(i).add(current);
					}
					else if(level[i]==(level[current]+1)) predecessor.get(i).add(current);
				}
			}
		}
	}
	void bfsForStep2(int src) {
		Queue<Integer> queue = new java.util.LinkedList<Integer>();
		boolean[] visited = new boolean[vertices];
		queue.add(src);
		visited[src] = true;
		while(!queue.isEmpty()) {
			int current = queue.remove();
			for(int i=0; i<vertices; i++) {
				if(newMat[current][i]==1 && level[i]>level[current]) noOfShortestPaths[i]++;
				if(newMat[current][i]==1 && !visited[i]) {
					queue.add(i);
					visited[i] = true;
				}
			}
		}
	}
	int[][] readMatrix() {
		int[][] nodes = new int[4039][4039];
		try (BufferedReader br = new BufferedReader(new FileReader("/Users/sejalpawar/Downloads/facebook_combined.txt"))) {
    		String line;
    		while ((line = br.readLine()) != null) {
       			String[] n = line.split(" ");
       			int src = Integer.parseInt(n[0]), dest = Integer.parseInt(n[1]);
       			if(src>=0 && src<4039 && dest>=0 && dest<4039)
       				nodes[src][dest] = 1;
    		}
		}
		catch(Exception e){}
		return nodes;
	}
	public static void main(String... args) {
		new GN().girvanNewman();
	}
}
