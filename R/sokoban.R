
sokoban<-function(){


  init<-function(){
    soko$stage<-0
    soko$width<-soko$height<-15
    soko$step<-1/soko$width

    soko$dir<-'a'

    soko$barrier <- 1
    soko$max_barrier <- 9


  }

  barrier_init <- function(barrier)
  {

    if(barrier>=1&barrier<=soko$max_barrier)
    {
      if(barrier==0)
      {
        soko$box_num <- 2

        soko$wall<-matrix(rep(0,soko$width*soko$height),nrow=soko$width)
        soko$wall[1:4,] <- 1; soko$wall[10:15,] <- 1; soko$wall[7,1:5] <- 1;
        soko$wall[,1:4] <- 1; soko$wall[,11:15] <- 1;

        soko$target<-data.frame(x=c(6,5),y=c(7,9))
        soko$target <- soko$target[order(soko$target$x, soko$target$y), ]
        rownames(soko$target) <- c(1:soko$box_num)

        soko$box<-data.frame(x=c(7,6),y=c(8,9))
        soko$people<-data.frame(x=9,y=5)
      }
      if(barrier==1)
      {
        soko$box_num <- 4

        soko$wall<-matrix(rep(0,soko$width*soko$height),nrow=soko$width)
        soko$wall[1:4,] <- 1; soko$wall[11:15,] <- 1;
        soko$wall[c(5,6,8,9,10),5] <- 1;soko$wall[c(5,6,8,9,10),6] <- 1;
        soko$wall[c(5,6),7] <- 1;soko$wall[c(9,10),8] <- 1;
        soko$wall[c(5,6,7,9,10),9] <- 1;soko$wall[c(5,6,7,9,10),10] <- 1;
        soko$wall[,1:4] <- 1; soko$wall[,11:15] <- 1;

        soko$target<-data.frame(x=c(7,5,8,10),y=c(5,8,10,7))
        soko$target <- soko$target[order(soko$target$x, soko$target$y), ]

        soko$box<-data.frame(x=c(7,7,8,9),y=c(6,8,8,7))
        soko$people<-data.frame(x=8,y=7)
      }
      if(barrier==2)
      {
        soko$box_num <- 3

        soko$wall<-matrix(rep(0,soko$width*soko$height),nrow=soko$width)
        soko$wall[1:5,] <- 1; soko$wall[12:15,] <- 1;
        soko$wall[c(7,9,10),7] <- 1;soko$wall[c(9,10),8] <- 1;
        soko$wall[c(7,9,10),9] <- 1;
        soko$wall[,1:5] <- 1; soko$wall[,12:15] <- 1;

        soko$target<-data.frame(x=c(6,6,8),y=c(6,10,11))
        soko$target <- soko$target[order(soko$target$x, soko$target$y), ]

        soko$box<-data.frame(x=c(8,8,9),y=c(7,10,11))
        soko$people<-data.frame(x=c(8),y=c(6))
      }
      if(barrier==3)
      {
        soko$box_num <- 4

        soko$wall<-matrix(rep(0,soko$width*soko$height),nrow=soko$width)
        soko$wall[1:4,] <- 1; soko$wall[11:15,] <- 1;
        soko$wall[5:8,5] <- 1;
        soko$wall[5:6,6] <- 1;soko$wall[9,7] <- 1;
        soko$wall[5:6,9] <- 1;soko$wall[5:8,10] <- 1;
        soko$wall[,1:4] <- 1; soko$wall[,11:15] <- 1;

        soko$target<-data.frame(x=c(5,5,6,7),y=c(7,8,8,9))
        soko$target <- soko$target[order(soko$target$x, soko$target$y), ]

        soko$box<-data.frame(x=c(7,8,9,9),y=c(7,8,8,9))
        soko$people<-data.frame(x=c(10),y=c(7))
      }
      if(barrier==4)
      {
        soko$box_num <- 3

        soko$wall<-matrix(rep(0,soko$width*soko$height),nrow=soko$width)
        soko$wall[1:4,] <- 1; soko$wall[11:15,] <- 1;
        soko$wall[5:7,5] <- 1;soko$wall[c(7,8),6] <- 1;
        soko$wall[c(5,7,8),8] <- 1;
        soko$wall[c(5,9),9] <- 1;soko$wall[5:7,10] <- 1;
        soko$wall[,1:4] <- 1; soko$wall[,11:15] <- 1;

        soko$target<-data.frame(x=c(8,9,10),y=c(5,5,5))
        soko$target <- soko$target[order(soko$target$x, soko$target$y), ]

        soko$box<-data.frame(x=c(6,9,10),y=c(7,6,9))
        soko$people<-data.frame(x=c(5),y=c(6))
      }
      if(barrier==5)
      {
        soko$box_num <- 3

        soko$wall<-matrix(rep(0,soko$width*soko$height),nrow=soko$width)
        soko$wall[1:4,] <- 1; soko$wall[12:15,] <- 1;
        soko$wall[c(5,6,11),5] <- 1;soko$wall[c(5,6,7,8,11),6] <- 1;
        soko$wall[c(5,6,7,8,10,11),7] <- 1;soko$wall[5:8,8] <- 1;
        soko$wall[8:9,10] <- 1;
        soko$wall[8:11,11] <- 1;
        soko$wall[,1:4] <- 1; soko$wall[,12:15] <- 1;

        soko$target<-data.frame(x=c(7,8,9),y=c(5,5,5))
        soko$target <- soko$target[order(soko$target$x, soko$target$y), ]

        soko$box<-data.frame(x=c(6,6,7),y=c(9,10,10))
        soko$people<-data.frame(x=c(5),y=c(11))
      }
      if(barrier==6)
      {
        soko$box_num <- 4

        soko$wall<-matrix(rep(0,soko$width*soko$height),nrow=soko$width)
        soko$wall[1:5,] <- 1; soko$wall[11:15,] <- 1;
        soko$wall[c(6,7,10),4] <- 1;
        soko$wall[7,6] <- 1;soko$wall[c(7,9,10),8] <- 1;
        soko$wall[6,10] <- 1;
        soko$wall[c(6,9,10),11] <- 1;
        soko$wall[,1:3] <- 1; soko$wall[,12:15] <- 1;

        soko$target<-data.frame(x=c(9,9,10,10),y=c(5,6,5,6))
        soko$target <- soko$target[order(soko$target$x, soko$target$y), ]

        soko$box<-data.frame(x=c(7,8,8,9),y=c(5,7,10,9))
        soko$people<-data.frame(x=c(8),y=c(5))
      }
      if(barrier==7)
      {
        soko$box_num <- 5

        soko$wall<-matrix(rep(0,soko$width*soko$height),nrow=soko$width)
        soko$wall[1:5,] <- 1; soko$wall[11:15,] <- 1;
        soko$wall[c(6,7,10),4] <- 1
        soko$wall[c(6,10),5] <- 1;soko$wall[c(6,10),6] <- 1;
        soko$wall[10,7] <- 1;soko$wall[c(7,10),8] <- 1;
        soko$wall[7,9] <- 1;
        soko$wall[c(6,7,9,10),11] <- 1;
        soko$wall[,1:3] <- 1; soko$wall[,12:15] <- 1;

        soko$target<-data.frame(x=c(8,9,7,8,9),y=c(4,4,5,5,5))
        soko$target <- soko$target[order(soko$target$x, soko$target$y), ]

        soko$box<-data.frame(x=c(7,8,8,9,9),y=c(7,6,8,7,9))
        soko$people<-data.frame(x=c(8),y=c(11))
      }
      if(barrier==8)
      {
        soko$box_num <- 5

        soko$wall<-matrix(rep(0,soko$width*soko$height),nrow=soko$width)
        soko$wall[1:4,] <- 1; soko$wall[11:15,] <- 1;
        soko$wall[5:9,4] <- 1;
        soko$wall[5:9,5] <- 1;soko$wall[5,6] <- 1;
        soko$wall[8,8] <- 1;
        soko$wall[c(5,6,8,9),9] <- 1;soko$wall[9:10,11] <- 1;
        soko$wall[,1:3] <- 1; soko$wall[,12:15] <- 1;

        soko$target<-data.frame(x=c(10,10,10,10,10),y=c(4,5,6,7,8))
        soko$target <- soko$target[order(soko$target$x, soko$target$y), ]

        soko$box<-data.frame(x=c(7,7,7,8,9),y=c(6,8,10,7,7))
        soko$people<-data.frame(x=c(5),y=c(11))
      }
      if(barrier==9)
      {
        soko$box_num <- 6

        soko$wall<-matrix(rep(0,soko$width*soko$height),nrow=soko$width)
        soko$wall[1:4,] <- 1; soko$wall[12:15,] <- 1;
        soko$wall[5:9,4] <- 1;soko$wall[8:9,6] <- 1;
        soko$wall[c(5,7),7] <- 1;soko$wall[c(5,7),8] <- 1;
        soko$wall[7,9] <- 1;soko$wall[8:9,10] <- 1;
        soko$wall[5:9,12] <- 1;
        soko$wall[,1:3] <- 1; soko$wall[,13:15] <- 1;

        soko$target<-data.frame(x=c(8,8,8,9,9,9),y=c(7,8,9,7,8,9))
        soko$target <- soko$target[order(soko$target$x, soko$target$y), ]

        soko$box<-data.frame(x=c(6,7,7,10,10,10),y=c(8,5,11,5,8,11))
        soko$people<-data.frame(x=c(11),y=c(11))
      }

    }
    else
    {
    }

    rownames(soko$target) <- c(1:soko$box_num)
  }

  check.x <- c(1,0,-1,0,1)
  check.y <- c(0,1,0,-1,0)

  success <- function()
  {
    soko$box <- soko$box[order(soko$box$x, soko$box$y), ]
    rownames(soko$box) <- c(1:soko$box_num)
    result <- identical(soko$box,soko$target)

    return(result)
  }

  move <- function(direction)
  {
    if(direction!=0){
      flag.box <- FALSE

      direc.x <- check.x[direction]
      direc.y <- check.y[direction]

      check.people.x <- soko$people$x+direc.x
      check.people.y <- soko$people$y+direc.y

      box_order <- 1
      while(box_order<=soko$box_num)
      {
        if(check.people.x==soko$box$x[box_order]&check.people.y==soko$box$y[box_order])
        {
          flag.box <- TRUE
          break
        }
        box_order <- box_order+1
      }

      if(flag.box==FALSE)
      {
        if(soko$wall[check.people.x,check.people.y]==0)
        {
          soko$people$x <- check.people.x
          soko$people$y <- check.people.y
        }
      }
      else
      {
        check.box.x <- check.people.x+direc.x
        check.box.y <- check.people.y+direc.y
        flag.box.box <- FALSE
        for(i in 1:soko$box_num)
        {
          if(check.box.x==soko$box$x[i]&check.box.y==soko$box$y[i])
          {
            flag.box.box <- TRUE
            break
          }
        }

        if(flag.box.box==FALSE)
        {
          if(soko$wall[check.box.x,check.box.y]==0)
          {
            soko$people$x <- check.people.x
            soko$people$y <- check.people.y
            soko$box$x[box_order] <- check.box.x
            soko$box$y[box_order] <- check.box.y
          }
        }
      }
    }
  }

  drawTable<-function(){
    plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  }

  whiteMatrix<-function(){

    plot_box <- (soko$box-1)/soko$width+soko$step/2
    symbols(plot_box$x,plot_box$y,rectangles=matrix(0.8*soko$step,length(plot_box$x),2),
            inches = FALSE,fg ="white",bg = NULL,add=TRUE)


    plot_people <- (soko$people-1)/soko$width+soko$step/2
    symbols(plot_people$x,plot_people$y,stars=matrix(0.5*soko$step,length(plot_people$x),6),
            inches = FALSE,fg ="white",bg = "white",add=TRUE)

  }

  drawMatrix<-function(){
    idx<-which(soko$wall==1)
    px<- (ifelse(idx%%soko$width==0,soko$width,idx%%soko$width)-1)/soko$width+soko$step/2
    py<- (ceiling(idx/soko$height)-1)/soko$height+soko$step/2
    pxy<-data.frame(x=px,y=py)
    symbols(px, py, rectangles = matrix(soko$step, length(px), 2),
            inches = FALSE, fg = colors()[267], bg = colors()[267], add = TRUE)

    plot_target <- (soko$target-1)/soko$width+soko$step/2
    symbols(plot_target$x,plot_target$y,circles = matrix(1/5*soko$step,length(plot_target$x)),
            inches = FALSE,fg =colors()[333],bg = colors()[573],add=TRUE)

    text(x =0.5, y = 0.95, labels = paste("No.",soko$barrier,"level"), col = colors()[404],cex=2)

    text(x =0.5, y = 0.05, labels = "R to restart, T return to start interface", col = colors()[404])

    plot_box <- (soko$box-1)/soko$width+soko$step/2
    symbols(plot_box$x,plot_box$y,rectangles=matrix(0.8*soko$step,length(plot_box$x),2),
            inches = FALSE,fg =colors()[565],bg = NA,add=TRUE)


    plot_people <- (soko$people-1)/soko$width+soko$step/2
    symbols(plot_people$x,plot_people$y,stars=matrix(0.5*soko$step,length(plot_people$x),6),
            inches = FALSE,fg =colors()[333],bg = colors()[373],add=TRUE)

  }

  stage1<-function(){
    soko$stage<-1

    whiteMatrix()

    if(soko$dir=='up') move(2)
    if(soko$dir=='down') move(4)
    if(soko$dir=='left') move(3)
    if(soko$dir=='right') move(1)
    if(soko$dir=='a') move(0)

    drawMatrix()

    if(success())
    {
      print("success")
      if(soko$barrier==soko$max_barrier)
      {
        soko$stage <- 2
        soko$barrier <- 1
      }
      else
      {
        soko$barrier <- soko$barrier+1
        barrier_init(soko$barrier)

        Sys.sleep(2)
        drawTable()
        drawMatrix()
      }
    }

  }

  stage0<-function(){
    soko$stage<-0
    plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
    text(0.5,0.8,label="Sokoban",cex=5)
    text(0.5,0.5,label="Any keyboard to start",cex=2,col=2)
    text(0.5,0.3,label="Up,Down,Left,Rigth to control direction",cex=1,col=4)
    text(0.5,0.2,label="Red : people, Squre : box, Orange: destination",cex=1,col=4)
  }

  stage2<-function(){
    plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
    text(0.5,0.5,label=paste("Congratulations!"),cex=5,col=4)
  }

  keydown<-function(K){
    print(paste("keydown:",K,",stage:",soko$stage));

    if(soko$stage==0){
      init()
      barrier_init(soko$barrier)

      drawTable()
      stage1()
      return(NULL)
    }

    if(soko$stage==2){
      stage2()
      return(NULL)
    }

    if(soko$stage==1){
      if(K == "q") {
        stage0()
      } else {
        if(tolower(K) %in% c("r","t","up","down","left","right")){
          if(tolower(K)=="r"){barrier_init(soko$barrier);drawTable();drawMatrix()}
          else
            if(tolower(K)=="t"){drawTable();stage0()}
          else
          {soko$dir<-tolower(K)
          stage1()
          }
        }
      }
    }
    return(NULL)

  }

  par(mai=rep(0,4),oma=rep(0,4))
  soko<<-new.env()
  stage0()
  getGraphicsEvent(prompt="Sokoban Game",onKeybd=keydown)
  soko <- NULL
}
