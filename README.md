# Clipper2
Polygon Clipping and Offsetting library

This is a beta version or the long awaited update to my <a href="https://sourceforge.net/projects/polyclipping/">Clipper Library</a><br>

Much of the code has been rewritten. Major changes include:
<ul>
  <li>both integer and floating point coordinates are now supported. The Library still performs all clipping operations using integer coordinates (to preserve numerical robustness), but floating point - integer coversions are now managed internally.
  <li>there's much simpler and more efficient removal of spikes and micro-self-intersections from clipping solutions.
  <li>and there's a modest improvement in performance too
</ul> 

More details can be found in the Docs folder.

![clipper_performance](https://user-images.githubusercontent.com/5280692/158306552-0c506e30-65db-4492-90b4-d962c0322bd7.png)
