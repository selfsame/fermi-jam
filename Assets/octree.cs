using UnityEngine;
using System.Collections;

public class octree : MonoBehaviour {

	public PointOctree<GameObject> pointTree;

	// Use this for initialization
	void Awake () {
		pointTree = new PointOctree<GameObject>(1000f, transform.position, 0.5f);
	}
	
	// Update is called once per frame
	void Update () {
	
	}
}
