
record = "300"
location = test_path()
sig <- read_wfdb(record = record, location = location)
detect_waveforms(record, location, detector = "/usr/bin/local/ecgpuwave")
ann <- read_annotation(record, location, extension = "ecgpuwave")

s <- 2
hz <- 360
n <- s*hz
xs <- sig[[1]][1:n, 1]
xd <- data.frame(sig = xs, index = 1:n)
xl <- data.frame(
	index = ann$sample[ann$sample < n],
	lab = ann$label[ann$sample < n],
	tw = ann$number[ann$sample < n]
)

xp <- data.frame(
	start = xl$index[which(xl$lab == "p") - 1],
	end = xl$index[which(xl$lab == "p") + 1],
	color = "#B2DFDA"
)

xr <- data.frame(
	start = xl$index[which(xl$lab == "N") - 1],
	end = xl$index[which(xl$lab == "N") + 1],
	color = "#009687"
)

xt <- data.frame(
	start = xl$index[which(xl$lab == "t") - 1],
	end = xl$index[which(xl$lab == "t") + 1],
	color = "#004C3F"
)

xecg <- rbind(xp, xr, xt)


ggplot() +
	theme_minimal() +
	geom_rect(
		data = xecg,
		aes(
			xmin = start,
			xmax = end,
			ymin = -Inf,
			ymax = Inf,
			fill = color
		),
		alpha = 0.4
	) +
	scale_fill_identity() +
	geom_line(data = xd, aes(x = index, y = sig))

