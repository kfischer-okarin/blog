function setDiffSlider (diff, percentage) {
  const slider = diff.querySelector('.diff-slider');
  const leftArea = slider.nextElementSibling;
  slider.style.marginLeft = `calc(-${100 - percentage}% - 5px)`;
  leftArea.style.width = `calc(${percentage}% - 5px)`;
  leftArea.style.marginLeft = `calc(-${percentage}% - 5px)`;
}

(() => {
  const diffs = document.querySelectorAll('.code-block.diff');
  diffs.forEach((diff) => {
    setDiffSlider(diff, 0);
    const slider = diff.querySelector('.diff-slider');
    const leftArea = slider.nextElementSibling;
    const rightArea = slider.previousElementSibling;

    slider.addEventListener('mousedown', (event) => {
      const slideArea = diff.getBoundingClientRect();

      leftArea.style.userSelect = 'none';
      rightArea.style.userSelect = 'none';

      const mousemoveListener = (event) => {
        const fromLeft = event.clientX - slideArea.left;
        const percentage = Math.min(
          Math.max(
            (fromLeft / slideArea.width) * 100,
            0
          ),
          100
        );
        setDiffSlider(diff, percentage);
      };

      const mouseupListener = () => {
        document.removeEventListener('mousemove', mousemoveListener);
        document.removeEventListener('mouseup', mouseupListener);
        leftArea.style.userSelect = 'auto';
        rightArea.style.userSelect = 'auto';
      };
      document.addEventListener('mousemove', mousemoveListener);
      document.addEventListener('mouseup', mouseupListener);
    });
  });
})();
