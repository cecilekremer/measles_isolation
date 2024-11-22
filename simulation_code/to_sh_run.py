import csv

rows = csv.DictReader(open("simSettings.csv"),delimiter=";")

qsub = "sbatch -M wice run_sim.slurm "

for row in rows:
    vars = []
    for key in row:
        vars.append(key + "=" + row[key])
    print("sbatch -M wice " + "--export \"" + ",".join(vars) + "\"" + " run_sim.slurm")
