# SymLearn Demo Setup
## Getting Started
The SymLearn demo runs two Docker containers, one for Redis, the other for the learner. 
The following commands should get you up and running:
```bash
git clone https://github.com/PhillipVH/symlearn.git
cd symlearn
git checkout feature/demo-mode
bash build.sh # Build the images
bash start.sh # Start the containers
```
Now you can head over to `http://localhost:3000/?parser=<parser>&depth=<depth>`, where `<parser>` is one of [`TacasParser`, `LearnLarge`, `PaperExample`], and `<depth>` is an integer (and preferably a small one at that, think between one and four, maybe five). After starting the containers, try out `PaperExample` at depth [one](http://localhost:3000/?parser=PaperExample&depth=1), [two](http://localhost:3000/?parser=PaperExample&depth=2), [three](http://localhost:3000/?parser=PaperExample&depth=4), and [four](http://localhost:3000/?parser=PaperExample&depth=4). 

> Note, for the time being, only interact with SymLearn via a single tab. Multiple tabs might cause race conditions in the underlying learners.
