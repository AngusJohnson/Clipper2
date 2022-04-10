# Clipper2
### A Polygon <a href="http://www.angusj.com/delphi/clipper/documentation/Docs/Units/ClipperLib/Types/ClipType.htm">Clipping</a> and <a href="http://www.angusj.com/delphi/clipper/documentation/Docs/Units/ClipperLib/Classes/ClipperOffset/_Body.htm">Offsetting</a> library

<b>Clipper2</b> is a major update of my original <a href="https://sourceforge.net/projects/polyclipping/"><b>Clipper</b></a> library which I'm now calling <b>Clipper1</b>. Clipper1 was written over 10 years ago and, while it still works very well, Clipper2 is better in just about every way: 
<ul>
<li>The code is cleaner and faster.</li>
<li>Clipper2 is easier to use with simple functions that handle all common uses.</li> 
<li>Floating point coordinates no longer need to be converted to integers before passing them into the library.</li>
<li>And micro self-intersections (integer rounding artifacts) and polygons with touching edges are now rarely seen in solutions.</li>
</ul>
</p>
<p class="Body"> 
And Clipper2 has just about all the features of Clipper1 that sets Clipper apart from other polygon clipping libraries, including: 
<ul>
<li>being able to clip complex self-intersecting polygons</li>
<li>support polygons with multiple filling rules (EvenOdd, NonZero, Positive, Negative)</li>
<li>is numerically robust, and</li>
<li>is free to use in both freeware and commercial applications</li>
</ul>
<br>

There's more detail in the Docs folder above, but proper documentation is still a little way off.

![clipper_performance](https://user-images.githubusercontent.com/5280692/162539497-316f719f-1c75-4638-bbec-02de5bfd87a5.png)
