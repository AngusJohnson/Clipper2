# Clipper2
Polygon Clipping and Offsetting library

This is a preview of a major update to my original <a href="https://sourceforge.net/projects/polyclipping/">Clipper Library</a><br>

Much of the code has been rewritten. Major changes include:
<ul>
  <li>both integer and floating point coordinates are now supported. The library still performs all clipping operations using integer coordinates (to preserve numerical robustness), but floating point - integer conversions are now managed internally.
  <li>there's now better (more efficient and more complete) removal of spikes and micro-self-intersections from clipping solutions.
  <li>and there's a modest improvement in performance too (see chart below)
</ul> 

There's more detail in the Docs folder above but proper documentation is still some way off.

![clipper_performance2](https://user-images.githubusercontent.com/5280692/160232945-0c44acdd-33b7-4efd-bd85-8338142148b0.png)

