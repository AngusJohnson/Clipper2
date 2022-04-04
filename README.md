# Clipper2
### A Polygon Clipping and Offsetting library

This is a preview of a major update to my original <a href="https://sourceforge.net/projects/polyclipping/">Clipper Library</a><br>

Much of the code has been rewritten. Major changes include:
<ul>
  <li>Both integer and floating point coordinates are now supported. While the library still performs all clipping operations using integer coordinates to preserve numerical robustness, conversions from floating point to integer (and back) can now be performed by the library.
  <li>There's more efficient and more complete removal of spikes and micro-self-intersections from clipping solutions, and much better merging of touching polygons too.
  <li>And there's a modest improvement in performance (see chart below)
</ul> 

There's more detail in the Docs folder above, but proper documentation is still some way off.

![clipper_performance](https://user-images.githubusercontent.com/5280692/161505265-45ba2081-62dc-4696-b06d-accb26aa3cb7.png)
