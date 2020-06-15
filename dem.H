      parameter (l1=260000,l2=50)

      common /d1/
     $       ts, r, e, ew,     ftbl, fn, fwn, mass,
     $       tp, rho, g, gw, tnomax, ft, fwt, inat,
     $       te, mu, nu, muw,        fb, fwb, cgn,
     $       dt, t, cf, k1max, cgs,

     $       iwall, xw, ewnx,ewtx,ewbx, vwx,
     $              yw, ewny,ewty,ewby, vwy,
     $              zw, ewnz,ewtz,ewbz, vwz,

     $       ip, x, vx, omgx, fx, mx,
     $           y, vy, omgy, fy, my,
     $           z, vz, omgz, fz, mz,

     $       upx, dpartx, parttl, kn, vn, vwn, knmx,
     $       upy, dparty, part,   ks, vt, vwt, ksmx,
     $       upz, dpartz, boxmax,     vb, vwb,
     $       ang, counth

      common /sdv/
     $       step,   sumfn, sumvx,vcnt, cx, vol,
     $       outcnt, sumft, sumvy,      cy, rang,
     $               sumfb, sumvz,      cz, v,ct

      integer
     $       ip,    px, upx, tno,    parttl(29,5,250),
     $       iwall, py, upy, tnomax, part(29,5,250,l2),
     $       h(l1),pz, upz, boxmax, outcnt, ftbl(l1,l2)

      integer ct

      real*8
     $       ts, r(260000), e, ew, fn(l1,l2), fwn(l1,l2), mass(260000),knmx,
     $       tp, rho,     g, gw, ft(l1,l2), fwt(l1,l2), inat(260000),ksmx,
     $       te, mu, nu, muw,    fb(l1,l2), fwb(l1,l2), cgn(l1),
     $       dt, t, cf, k1max, cgs(l1), counth(l2),

     $              xw(l2), ewnx(l2),ewtx(l2),ewbx(l2), vwx(l2),
     $              yw(l2), ewny(l2),ewty(l2),ewby(l2), vwy(l2),
     $              zw(l2), ewnz(l2),ewtz(l2),ewbz(l2), vwz(l2),

     $           x(l1), vx(l1), omgx(l1), fx(l1), mx(l1),
     $           y(l1), vy(l1), omgy(l1), fy(l1), my(l1),
     $           z(l1), vz(l1), omgz(l1), fz(l1), mz(l1),

     $        dpartx, kn, vn(l1,l2), vwn(l1,l2),
     $        dparty, ks, vt(l1,l2), vwt(l1,l2),
     $        dpartz,     vb(l1,l2), vwb(l1,l2)

      real*8
     $       step,   sumfn( 8,16,70), sumvx(16,3,70), cx, vcnt(16,3,70),
     $               sumft( 8,16,70), sumvy(16,3,70), cy, rang,
     $               sumfb( 8,16,70), sumvz(16,3,70), cz, v,
     $       vol(16,3,70)
      real*8
     $       xo,yo,ang
