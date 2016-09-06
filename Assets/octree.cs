using UnityEngine;
using System.Collections;

public class octree : MonoBehaviour {

	public PointOctree<GameObject> pointTree;

	// Use this for initialization
	void Awake () {
		pointTree = new PointOctree<GameObject>(15, transform.position, 1);
	}
	
	// Update is called once per frame
	void Update () {
	
	}
}
